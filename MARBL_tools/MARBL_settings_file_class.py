import logging

class MARBL_settings_class(object):
    """ This class contains methods to allow python to interact with the YAML file that
        defines the MARBL parameters and sets default values.

        This file also contains several subroutines that are not part of the class but are
        called by member functions in the class.
    """

    ###############
    # CONSTRUCTOR #
    ###############

    def __init__(self, default_settings_file, grid, input_file, file_is_JSON=False):
        """ Class constructor: set up a dictionary of config keywords for when multiple
            default values are provided, and then read the YAML file.
        """

        logger = logging.getLogger(__name__)

        # 1. List of configuration keywords to match in YAML if default_default is a dictionary
        self._config_keyword = []
        self._config_keyword.append("grid = " + grid)

        # 2. Read settings file
        if (file_is_JSON):
            import json
            with open(default_settings_file) as parmsfile:
                self._parms = json.load(parmsfile)
        else:
            try:
                import yaml
            except:
                logger.error("Can not find PyYAML library")
                _abort(1)
            try:
                with open(default_settings_file) as parmsfile:
                    self._parms = yaml.safe_load(parmsfile)
            except:
                logger.error("Can not find %s" % default_settings_file)
                _abort(1)

        # 3 Make sure YAML file adheres to MARBL parameter file schema
        if _invalid_parms_file(self._parms):
            logger.error("%s is not a valid MARBL parameter file" % default_settings_file)
            _abort(1)

        # 4. Read input file
        self._input_dict = _parse_input_file(input_file)

        # 5. Use an ordered dictionary for keeping variable, value pairs
        from collections import OrderedDict
        self.parm_dict = OrderedDict()
        for cat_name in self.get_category_names():
            for var_name in self.get_variable_names(cat_name):
                self._process_variable_value(cat_name, var_name)

        # 6. Abort if not all values from input file were processed
        #    (That implies at least one variable from input file was not recognized)
        if (self._input_dict):
            message = "Did not fully parse input file:"
            for varname in self._input_dict.keys():
                message = message + "\n     * Variable %s not found in YAML" % varname
            logger.error(message)
            _abort(1)

    ################################################################################
    #                             PUBLIC CLASS METHODS                             #
    ################################################################################

    def get_tracer_cnt(self):
        """ Return the number of tracers MARBL is running with.
        """

        return (self._parms['_tracer_cnt']['default'] +
                _add_increments(self._parms['_tracer_cnt']['increments'], self.parm_dict))

    ################################################################################

    def get_category_names(self):
        """ Returns category names as determined by the '_order' key in YAML
        """

        # Consistency checks:
        # 1. All keys listed in self._parms["_order"] should also be in self._parms.keys()
        for key in self._parms["_order"]:
            if key not in self._parms.keys():
                msg = "ERROR: can not find '" + key + "' in YAML file"
                _abort(msg)

        # 2. All keys listed in self._parms.keys() should also be in self._parms["_order"]
        #    (except _order itself)
        for key in self._parms.keys():
            if key not in ["_order", "_tracer_cnt"] and key not in self._parms["_order"]:
                msg = "ERROR: '" + key + "' is not listed in '_order' and won't be processed"
                _abort(msg)

        # 3. No duplicates in _order
        unique_keys = []
        for key in self._parms["_order"]:
            if key in unique_keys:
                msg = "ERROR: '" + key + "' appears in '_order' multiple times"
                _abort(msg)
            unique_keys.append(key)

        return self._parms["_order"]

    ################################################################################

    def get_subcategory_names(self):
        """ Returns a sorted list of subcategories in a specific category.
            For now, the list is sorted naturally (so 10 appears after 9, not after 1).

            Optional: only return variables in a specific subcategory
        """
        subcat_list = []
        for cat_name in self._parms['_order']:
            for var_name in _sort(self._parms[cat_name].keys()):
                if isinstance(self._parms[cat_name][var_name]['datatype'], dict):
                    for subvar_name in _sort(self._parms[cat_name][var_name]['datatype'].keys()):
                        if subvar_name[0] != '_':
                            this_subcat = self._parms[cat_name][var_name]['datatype'][subvar_name]['subcategory']
                            if this_subcat not in subcat_list:
                                subcat_list.append(this_subcat)
                else:
                    this_subcat = self._parms[cat_name][var_name]['subcategory']
                    if this_subcat not in subcat_list:
                        subcat_list.append(this_subcat)
        return _sort(subcat_list, sort_key=_natural_sort_key)

    ################################################################################

    def get_variable_names(self, category_name, sort_key=None):
        """ Returns a sorted list of variables in a specific category.
            For now, the list is sorted alphabetically.
        """
        return _sort(self._parms[category_name].keys(), sort_key)

    ################################################################################

    def get_parm_dict_variable_names(self, subcategory):
        """ Returns a sorted list of variables in a specific category
            and subcategory, expanding variable names if they are arrays
            or derived types
        """
        varlist = []
        for cat_name in self._parms['_order']:
            for var_name in _sort(self._parms[cat_name].keys()):
                if isinstance(self._parms[cat_name][var_name]['datatype'], dict):
                    for subvar_name in _sort(self._parms[cat_name][var_name]['datatype'].keys()):
                        if subvar_name[0] != '_':
                            this_var = self._parms[cat_name][var_name]['datatype'][subvar_name]
                            if this_var['subcategory'] == subcategory:
                                for parm_key in this_var['_list_of_parm_names']:
                                    varlist.append(parm_key)
                else:
                    this_var = self._parms[cat_name][var_name]
                    if this_var['subcategory'] == subcategory:
                        for parm_key in this_var['_list_of_parm_names']:
                            varlist.append(parm_key)
        return _sort(varlist, sort_key=_natural_sort_key)

    ################################################################################
    #                            PRIVATE CLASS METHODS                             #
    ################################################################################

    # TODO: define _value_is_valid()
    #       i.  datatype match?
    #       ii. optional valid_value key check

    def _process_variable_value(self, category_name, variable_name):
        """ For a given variable in a given category, call _update_parm_dict()
            * If variable is a derived type, _update_parm_dict() needs to be called element by element

            NOTE: At this time, the only derived types in the YAML file are also arrays
        """
        this_var = self._parms[category_name][variable_name]

        if not isinstance(this_var["datatype"], dict):
            this_var['_list_of_parm_names'] = []
            self._update_parm_dict(this_var, variable_name)
            return

        # Process derived type!
        append_to_keys = (('PFT_defaults = "CESM2"' in self._config_keyword) and
                          (category_name == "PFT_derived_types"))
        if append_to_keys:
            PFT_keys = self._parms['general_parms']['PFT_defaults']['_CESM2_PFT_keys'][variable_name]
        # Is the derived type an array? If so, treat each entry separately
        if ("_array_size" in this_var.keys()):
            for n, elem_index in enumerate(_get_array_info(this_var["_array_size"], self.parm_dict)):
                # Append "(index)" to variable name
                base_name = "%s%s%%" % (variable_name, elem_index)

                if append_to_keys:
                    # Add key for specific PFT
                    self._config_keyword.append('%s = "%s"' % (variable_name, PFT_keys[n]))

                for key in _sort_with_specific_suffix_first(this_var["datatype"].keys(),'_cnt'):
                    if key[0] != '_':
                        # Call _update_parm_dict() for each variable in derived type
                        this_component = this_var["datatype"][key]
                        try:
                            this_component['_list_of_parm_names']
                        except:
                            this_component['_list_of_parm_names'] = []
                        self._update_parm_dict(this_component, base_name+key, base_name)

                if append_to_keys:
                    # Remove PFT-specific key
                    del self._config_keyword[-1]

    ################################################################################

    def _update_parm_dict(self, this_var, var_name, base_name=''):
        """ For a given variable in a given category, add to the self.parm_dict dictionary
            * For derived types, user passes in component as well as base_name ("variable_name%")
            * For arrays, multiple entries will be added to self.parm_dict

            Also introduce a new key to the variable dictionary, '_list_of_parm_names', that
            is populated with a list of all the keys added to self.parm_dict for this variable
            (just varname for scalars, but multiple keys for arrays)
        """
        if ("_array_size" in this_var.keys()):
            # Get length of array
            try:
                array_len = this_var["_array_len_to_print"]
            except:
                array_len = this_var["_array_size"]

            # For each element, get value from either input file or YAML
            for n, elem_index in enumerate(_get_array_info(array_len, self.parm_dict, base_name)):
                full_name = var_name + elem_index
                var_value = _get_var_value(full_name, this_var, self._config_keyword, self._input_dict)
                if isinstance(var_value, list):
                    if this_var["datatype"] == "string" and n>=len(var_value):
                        self.parm_dict[full_name] = '""'
                    else:
                        self.parm_dict[full_name] = _translate_YAML_value(var_value[n], this_var["datatype"])
                else:
                    self.parm_dict[full_name] = var_value
                this_var['_list_of_parm_names'].append(full_name)

        else:
            # get value from either input file or YAML
            self.parm_dict[var_name] = _get_var_value(var_name, this_var, self._config_keyword, self._input_dict)
            this_var['_list_of_parm_names'].append(var_name)

################################################################################
#                            PRIVATE MODULE METHODS                            #
################################################################################

def _abort(err_code=0):
    """ This routine imports sys and calls exit
    """
    import sys
    sys.exit(err_code)

################################################################################

def _invalid_parms_file(YAMLdict):
    """ Read a YAML file, make sure it conforms to MARBL parameter file standards
        1. _order is a top-level key
        2. Everything listed in _order is a top-level key
        3. All top-level keys that do not begin with '_' are listed in _order
        4. All second-level dictionaries (variable names) contain datatype key
        5. If datatype is not a dictionary, variable dictionary keys also included
           longname, subcategory, units, default_value
        6. If datatype is a dictionary, all keys in the datatype are variables per (5)
        7. In a variable (or datatype entry) where default_value is a dictionary,
           "default" is a key
        NOTE: (7) is checked explicitly along with (5) and (6) in _valid_variable_dict()
    """

    invalid_file = False
    logger = logging.getLogger(__name__)

    # 1. _order is a top-level key
    if "_order" not in YAMLdict.keys():
        logger.error("Can not find _order key")
        return True

    # 2. Everything listed in _order is a top-level key
    for cat_name in YAMLdict["_order"]:
        if cat_name not in YAMLdict.keys():
            logger.error("Can not find %s category that is listed in _order" % cat_name)
            invalid_file = True

    for cat_name in YAMLdict.keys():
        if cat_name[0] != '_':
        # 3. All top-level keys that do not begin with '_' are listed in _order
            if cat_name not in YAMLdict["_order"]:
                logger.error("Category %s not included in _order" % cat_name)
                invalid_file = True

            # 4. All second-level dictionaries (variable names) contain datatype key
            #    If the variable is of a derived type, then datatype is a dictionary itself
            for var_name in YAMLdict[cat_name].keys():
                if "datatype" not in YAMLdict[cat_name][var_name].keys():
                    logger.error("Variable %s does not contain a key for datatype" % var_name)
                    invalid_file = True
                    continue

                if not isinstance(YAMLdict[cat_name][var_name]["datatype"], dict):
                    # 5. If datatype is not a dictionary, variable dictionary keys should include
                    #    longname, subcategory, units, datatype, default_value
                    #    Also, if default_value is a dictionary, that dictionary needs to contain "default" key
                    if not _valid_variable_dict(YAMLdict[cat_name][var_name], var_name):
                        invalid_file = True
                else:
                    # 6. If datatype is a dictionary, all keys in the datatype are variables per (5)
                    for subvar_name in YAMLdict[cat_name][var_name]["datatype"].keys():
                        if subvar_name[0] != '_':
                            if not _valid_variable_dict(YAMLdict[cat_name][var_name]["datatype"][subvar_name],
                                                        "%s%%%s"  % (var_name, subvar_name)):
                                invalid_file = True

    return invalid_file

################################################################################

def _valid_variable_dict(var_dict, var_name):
    """ Return False if dictionary does not contain any of the following:
        * longname
        * subcategory
        * units
        * datatype
        * default_value
    """

    logger = logging.getLogger(__name__)
    for key_check in ["longname", "subcategory", "units", "datatype", "default_value"]:
        if key_check not in var_dict.keys():
            message = "Variable %s is not well-defined in YAML" % var_name
            message = message + "\n     * Expecting %s as a key" % key_check
            logger.error(message)
            return False
    if isinstance(var_dict["default_value"], dict):
        # Make sure "default" is a valid key if default_value is a dictionary
        if "default" not in var_dict["default_value"].keys():
            logger.error("default_value dictionary in variable %s must have 'default' key" % var_name)
            logger.info("Keys in default_value are %s" % var_dict["default_value"].keys())
            return False
    return True

################################################################################

def _get_var_value(varname, var_dict, provided_keys, input_dict):
    """ Return the correct default value for a variable in the MARBL YAML parameter
        file INPUTS:
            * dictionary containing variable information (req: longname, datatype
              and default_value keys)
            * list of keys to try to match in default_value
            * dictionary containing values from input file
    """
    # Either get value from input file or from the YAML
    if varname in input_dict.keys():
        # Ignore ' and " from strings
        def_value = input_dict[varname].strip('"').strip("'")
        # Remove from input file dictionary; if dictionary is not empty after processing
        # all input file lines, then it included a bad variable in it
        del input_dict[varname]
    # Note that if variable foo is an array, then foo = bar in the input file
    # should be treated as foo(1) = bar
    elif varname[-3:] == "(1)" and varname[:-3] in input_dict.keys():
        def_value = input_dict[varname[:-3]].strip('"').strip("'")
        # Remove from input file dictionary; if dictionary is not empty after processing
        # all input file lines, then it included a bad variable in it
        del input_dict[varname[:-3]]
    else:
        # is default value a dictionary? If so, it depends on self._config_keyword
        # Otherwise we're interested in default value
        if isinstance(var_dict["default_value"], dict):
            # NOTE: _invalid_parms_file() has ensured that this dictionary has a "default" key
            use_key = "default"
            for key in provided_keys:
                # return "default" entry in default_values dictionary unless one of the keys
                # in provided_keys matches
                if key in var_dict["default_value"].keys():
                    use_key = key
            def_value = var_dict["default_value"][use_key]
        else:
            def_value = var_dict["default_value"]

    # call value validation check

    # Append to config keywords if YAML wants it

    # if default value is a list, return the whole thing
    if isinstance(def_value, list):
        return def_value

    if "_append_to_config_keywords" in var_dict.keys():
        append_to_config = var_dict["_append_to_config_keywords"]
    else:
        append_to_config = False
    return _translate_YAML_value(def_value, var_dict["datatype"], append_to_config, varname, provided_keys)

################################################################################

def _translate_YAML_value(value, datatype, append_to_config=False, varname=None, provided_keys=None):
    """ The value provided in the YAML file needs to be adjusted depending on the datatype
        of the variable. Strings need to be wrapped in "", and numbers written in
        scientific notation need to be formatted consistently.

        Also, some values need to added to the "provided_keys" list (by default assume that
        is not the case)
    """
    if isinstance(value, str):
        value = value.decode('utf-8')
    # if variable is a string, put quotes around the default value
    if datatype == "string":
        if append_to_config:
            provided_keys.append('%s = "%s"' % (varname, value))
        return '"%s"' % value.encode('utf-8')
    if datatype == "real" and isinstance(value, unicode):
        return "%24.16e" % eval(value)
    if datatype == "integer" and isinstance(value, unicode):
        return int(value)
    return value

################################################################################

def _sort(list_in, sort_key=None):
    """ Sort a list; default is alphabetical (case-insensitive), but that
        can be overridden with the sort_key argument
    """
    if sort_key is None:
        sort_key = lambda s: s.lower()
    return sorted(list_in, key=sort_key)

################################################################################

def _sort_with_specific_suffix_first(list_in, suffix=None, sort_key=None):
    """ Sort, but make sure entries that end in a specified suffix are listed first
    """

    # 1. initialize empty list
    list_out = []

    # 2. Anything that ends in suffix gets appended to list_out first
    if suffix is not None:
        for list_entry in _sort(list_in, sort_key):
            if list_entry.endswith(suffix):
                list_out.append(list_entry)

    # 3. Sort everything else
    for list_entry in _sort(list_in, sort_key):
        if list_entry not in list_out:
            list_out.append(list_entry)
    return list_out

################################################################################

def _natural_sort_key(string_):
    """ From https://stackoverflow.com/questions/2545532/python-analog-of-natsort-function-sort-a-list-using-a-natural-order-algorithm/3033342#3033342
    """
    import re
    return [int(s) if s.isdigit() else s for s in re.split(r'(\d+)', string_)]

################################################################################

def _add_increments(increments, parm_dict):
    """ Some values need to be adjusted depending on values in parm_dict
    """
    change = 0
    for key_check in increments.keys():
        checklist = key_check.split(" = ")
        if parm_dict[checklist[0]] == checklist[1]:
            change = change + increments[key_check]
    return change

################################################################################

def _get_dim_size(dim_in, parm_dict, dict_prefix=''):
    """ If dim_in is an integer, it is the dimension size. Otherwise we need to
        look up the dim_in key in parm_dict.
    """

    if isinstance(dim_in, dict):
        dim_start = dim_in['default']
        check_increment = ('increments' in dim_in.keys())
    else:
        dim_start = dim_in
        check_increment = False

    if isinstance(dim_start, int):
        # If dim_start is an integer, use it as dim_out
        dim_out = dim_start
    else:
        # Otherwise, dim_start must refer to a variable that could be
        # in the dictionary with or without the prefix
        try:
            dim_out = parm_dict[dict_prefix+dim_start]
        except:
            dim_out = parm_dict[dim_start]

    if check_increment:
        dim_out = dim_out + _add_increments(dim_in['increments'], parm_dict)

    return dim_out
################################################################################

def _get_array_info(array_size_in, parm_dict, dict_prefix=''):
    """ Return a list of the proper formatting for array elements, e.g.
            ['(1)', '(2)'] for 1D array or
            ['(1,1)', '(2,1)'] for 2D array
        If array_size_in is not an integer, check to see if it is in parm_dict
    """

    logger = logging.getLogger(__name__)

    # List to be returned:
    str_index = []

    # How many dimensions?
    if isinstance(array_size_in, list):
        # Error checking:
        # This script only support 2D arrays for now
        # (and assumes array_size_in is not a list for 1D arrays)
        if len(array_size_in) > 2:
            logger.error("_get_array_info() only supports 1D and 2D arrays")
            _abort(1)

        for i in range(0, _get_dim_size(array_size_in[0], parm_dict, dict_prefix)):
            for j in range(0, _get_dim_size(array_size_in[1], parm_dict, dict_prefix)):
                str_index.append("(%d,%d)" % (i+1,j+1))
        return str_index

    # How many elements? May be an integer or an entry in self.parm_dict
    for i in range(0, _get_dim_size(array_size_in, parm_dict, dict_prefix)):
        str_index.append("(%d)" % (i+1))
    return str_index

################################################################################

def _string_to_substring(str_in, separator):
    """ Basically the python native split() function, but ignore separator that
        is inside quotes. So (using separator = ',')
            'abc, def, gh' -> ['abc', 'def', 'gh']
        but
            'abc,"def, gh"' -> ['abc', '"def, gh"']
            "abc,'def, gh'" -> ['abc', '"def, gh"']

        Note: unexpected results if str_in is missing a closing ' or "
    """

    import re
    re_separator = separator+"(?=(?:[^\"\']|[\"|\'][^\"\']*[\"|\'])*$)"
    return re.split(re_separator, str_in)

################################################################################

def _parse_input_file(input_file):
    """ 1. Read an input file; ignore blank lines and non-quoted Fortran comments.
        2. Turn lines of the form
              variable = value
           Into input_dict['variable'] = value
        3. Return input_dict
    """
    input_dict = dict()
    logger = logging.getLogger(__name__)

    try:
        f = open(input_file, "r")
        for line in f:
            # Ignore comments in input file!
            line_loc = _string_to_substring(line, '!')[0]

            # ignore empty lines
            if len(line_loc.lstrip()) == 0:
                continue

            line_list = line_loc.strip().split('=')
            var_name = line_list[0].strip()
            value = line_list[1].strip()
            val_array = _string_to_substring(value, ',')
            if len(val_array) > 1:
                # Treat comma-delimited value as an array
                for n, value in enumerate(val_array):
                    suffix = "(%d)" % (n+1)
                    input_dict[var_name+suffix] = value.strip()
            else:
                # Single value
                input_dict[var_name] = value
        f.close()
    except TypeError:
        # If inputfile == None then the open will result in TypeError
        pass
    except:
        logger.error("input_file '%s' was not found" % input_file)
        _abort(1)
    return input_dict

################################################################################
