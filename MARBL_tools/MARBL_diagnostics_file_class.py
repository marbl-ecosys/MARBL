
import logging

class MARBL_diagnostics_class(object):
    """ This class contains methods to allow python to interact with the JSON file that
        defines the MARBL diagnostics and sets default values. It requires a
        MARBL_settings_class object upon construction because some diagnostics are only
        available with specific settings (for example, there are several carbon isotope
        diagnostics but MARBL will only compute them if ciso_on = .true.)
    """

    ###############
    # CONSTRUCTOR #
    ###############

    def __init__(self, default_diagnostics_file, MARBL_settings):
        """ Class constructor: set up a dictionary of config keywords for when multiple
            default values are provided, and then read the JSON file.
        """

        logger = logging.getLogger(__name__)

        # 1. Read diagnostics JSON file
        import json
        with open(default_diagnostics_file) as diagnostics_file:
            self._diagnostics = json.load(diagnostics_file)

        # 2. Make sure JSON file adheres to MARBL diagnostics file schema
        from MARBL_tools import diagnostics_dictionary_is_consistent
        if not diagnostics_dictionary_is_consistent(self._diagnostics):
            logger.error("%s is not a valid MARBL diagnostics file" % default_diagnostics_file)
            _abort(1)

        # 3. Generate processed version of self._diagnostics
        self.diagnostics_dict = self._process_diagnostics(MARBL_settings)

    ################################################################################
    #                            PRIVATE CLASS METHODS                             #
    ################################################################################

    def _process_diagnostics(self, MARBL_settings):
        """ This subroutine processes the dictionary read in from JSON.
            1. expand per-tracer / per-PFT diagnostics
            2. ignore diags where dependencies are not met
            3. single freq / op should be converted to list with single entry
        """
        # use regular expressions to find substrings wrapped in '(())'
        import re

        processed_dict = dict()
        for diag_name in self._diagnostics.keys():
            # 1. Expand per-tracer / per-PFT diagnostics
            #    [Look for keywords in (()) to signify need for string replacement]
            if re.search('\(\(.*\)\)', diag_name) == None:
                processed_dict[diag_name] = dict(self._diagnostics[diag_name])
            else:
                # (Also determine correct frequency if 'frequency' is a dict)
                _expand_template_value(diag_name, MARBL_settings, self._diagnostics[diag_name], processed_dict)

        for diag_name in processed_dict.keys():
            # 2. Delete diagnostics where dependencies are not met
            remove_diag = False
            if 'dependencies' in processed_dict[diag_name].keys():
                for dependency in processed_dict[diag_name]['dependencies'].keys():
                    if dependency in MARBL_settings.settings_dict.keys():
                        if processed_dict[diag_name]['dependencies'][dependency] != MARBL_settings.settings_dict[dependency]:
                            remove_diag = True
            if remove_diag:
                del processed_dict[diag_name]
                continue

            # 3. frequency and operator should always be lists
            if not isinstance(processed_dict[diag_name]['frequency'], list):
                processed_dict[diag_name]['frequency'] = [processed_dict[diag_name]['frequency']]
                processed_dict[diag_name]['operator'] = [processed_dict[diag_name]['operator']]

        return processed_dict

################################################################################

################################################################################
#                            PRIVATE MODULE METHODS                            #
################################################################################

def _abort(err_code=0):
    """ This routine imports sys and calls exit
    """
    import sys
    sys.exit(err_code)

################################################################################

def _expand_template_value(diag_name, MARBL_settings, unprocessed_entry, processed_dict):
    """ unprocessed_entry is a dictionary whose keys / values may have templated strings
        (e.g. strings that depend on tracer name or PFT name). This subroutine replaces
        the templated values and adds the appropriate entry / entries to processed_dict

        Also, if 'frequency' is a dictionary, determine correct default value!
    """

    import re

    logger = logging.getLogger(__name__)

    template = re.search('\(\(.*\)\)', diag_name).group()
    template_fill_dict = dict()
    if template == '((tracer_short_name))':
        fill_source = 'tracers'
        # diag name will replace template with key_fill_vals
        loop_for_replacement = MARBL_settings.settings_dict["_tracer_dict"].keys()
    elif template == '((autotroph_sname))':
        fill_source = 'autotrophs'
        loop_for_replacement = range(1,MARBL_settings.settings_dict['autotroph_cnt']+1)
    else:
        logger.error("%s is not a valid template value" % template)
        _abort(1)

    for item in loop_for_replacement:
        if fill_source == 'tracers':
            key_fill_val = item
            # more metadata will be available in template_fill_dict
            template_fill_dict['((tracer_long_name))'] = MARBL_settings.settings_dict["_tracer_dict"][key_fill_val]["long_name"]
            template_fill_dict['((tracer_tend_units))'] = MARBL_settings.settings_dict["_tracer_dict"][key_fill_val]["tend_units"]
            # Check to see if tracer is in tracer_restore_vars(:)
            template_fill_dict['((restore_this_tracer))'] = False
            for n in range(1,MARBL_settings.get_tracer_cnt()+1):
                if key_fill_val == MARBL_settings.settings_dict["tracer_restore_vars(%d)" % n].strip('"'):
                    template_fill_dict['((restore_this_tracer))'] = True
                    break
        elif fill_source == 'autotrophs':
            key_fill_val = MARBL_settings.settings_dict["autotrophs(%d)%%sname" % item].strip('"')
            template_fill_dict['((autotroph_lname))'] = MARBL_settings.settings_dict["autotrophs(%d)%%lname" % item].strip('"')
            # Autotroph properties
            imp_calcifier = (MARBL_settings.settings_dict["autotrophs(%d)%%imp_calcifier" % item].strip('"'))
            exp_calcifier = (MARBL_settings.settings_dict["autotrophs(%d)%%exp_calcifier" % item].strip('"'))
            silicifier = (MARBL_settings.settings_dict["autotrophs(%d)%%silicifier" % item].strip('"'))
            Nfixer = (MARBL_settings.settings_dict["autotrophs(%d)%%Nfixer" % item].strip('"'))
            template_fill_dict['((autotroph_calcifier))'] = ".true." in [imp_calcifier, exp_calcifier]
            template_fill_dict['((autotroph_silicifier))'] = (silicifier == ".true.")
            template_fill_dict['((autotroph_Nfixer))'] = (Nfixer == ".true.")
            #silicifier = MARBL_settings.settings_dict["autotrophs(%d)%%silicifier" % item]
        new_diag_name = diag_name.replace(template, key_fill_val)
        processed_dict[new_diag_name] = dict()
        for key in unprocessed_entry.keys():
            if not isinstance(unprocessed_entry[key], dict):
                # look for templates in values
                if isinstance(unprocessed_entry[key], type(u'')):
                    if re.search('\(\(.*\)\)', unprocessed_entry[key]) == None:
                        processed_dict[new_diag_name][key] = unprocessed_entry[key]
                    else:
                        template2 = re.search('\(\(.*\)\)', unprocessed_entry[key]).group()
                        try:
                            replacement_text = template_fill_dict[template2]
                        except:
                            logger.error("Can not replace '%s'" % template2)
                            _abort(1)
                        processed_dict[new_diag_name][key] = unprocessed_entry[key].replace(template2, replacement_text)
                else:
                    processed_dict[new_diag_name][key] = unprocessed_entry[key]
            else:
                if key == 'dependencies':
                    # need to check dependencies on a per-diagnostic basis
                    for dependency in unprocessed_entry['dependencies'].keys():
                        if dependency in template_fill_dict.keys():
                            check_val = template_fill_dict[dependency]
                        else:
                            try:
                                check_val = MARBL_settings.settings_dict[dependency]
                            except:
                                logger.error("Unknown dependency '%s'" % dependency)
                                _abort(1)
                        if unprocessed_entry['dependencies'][dependency] != check_val:
                            del processed_dict[new_diag_name]
                            return

                elif key == 'frequency':
                    dict_key = 'default'
                    for new_key in unprocessed_entry[key].keys():
                        #if new_key == '((restore_this_tracer))':
                        if new_key in template_fill_dict.keys():
                            if template_fill_dict[new_key]:
                                dict_key = new_key
                                break
                    processed_dict[new_diag_name][key] = unprocessed_entry[key][dict_key]
                else:
                    logger.error("Not expecting '%s' key to be a dictionary" % key)
                    _abort(1)

################################################################################

def _sort(list_in, sort_key=None):
    """ Sort a list; default is alphabetical (case-insensitive), but that
        can be overridden with the sort_key argument
    """
    if sort_key is None:
        sort_key = lambda s: s.lower()
    return sorted(list_in, key=sort_key)

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
    return [x.strip() for x in re.split(re_separator, str_in)]

################################################################################

def _parse_input_file(input_file):
    """ 1. Read an input file; ignore blank lines and non-quoted Python comments.
        2. Turn lines of the form
              diagnostic_name : frequency[, frequency2, ..., frequencyN]
           Into input_dict['diagnostic_name'] = frequency
        3. Return input_dict
    """
    input_dict = dict()
    logger = logging.getLogger(__name__)

    try:
        f = open(input_file, "r")
        for line in f:
            # Ignore comments in input file!
            line_loc = _string_to_substring(line, '#')[0]

            # ignore empty lines
            if len(line_loc.lstrip()) == 0:
                continue

            line_list = line_loc.strip().split(':')
            var_name = line_list[0].strip()
            value = line_list[1].strip()
            val_array = _string_to_substring(value, ',')
            if len(val_array) > 1:
                input_dict[var_name] = val_array
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
