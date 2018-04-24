
#!/usr/bin/env python

""" General tools that are used by multiple python files in this package
"""

################################################################################
#                            PUBLIC MODULE METHODS                             #
################################################################################

def settings_dictionary_is_consistent(SettingsDict):
    """ Make sure dictionary generated from JSON settings file conforms to MARBL
        parameter file standards:
        1. _order is a top-level key
        2. Everything listed in _order is a top-level key
        3. All top-level keys that do not begin with '_' are listed in _order
        4. All second-level dictionaries (variable names) contain datatype key
        5a.  If datatype is not a dictionary, variable dictionary contain the following keys:
             i.   longname
             ii.  subcategory
             iii. units
             iv.  datatype [checked in 4.]
             v.   default_value
        5b. If datatype is a dictionary, all keys in the datatype are variables per (5a)
        6. In a variable (or datatype entry) where default_value is a dictionary,
           "default" is a key
        NOTE: (5) and (6) are checked in _valid_variable_dict()
    """

    import logging
    logger = logging.getLogger(__name__)
    invalid_file = False

    # 1. _order is a top-level key
    if "_order" not in SettingsDict.keys():
        logger.error("Can not find _order key")
        return True

    # 2. Everything listed in _order is a top-level key
    for cat_name in SettingsDict["_order"]:
        if cat_name not in SettingsDict.keys():
            logger.error("Can not find %s category that is listed in _order" % cat_name)
            invalid_file = True

    for cat_name in SettingsDict.keys():
        if cat_name[0] != '_':
        # 3. All top-level keys that do not begin with '_' are listed in _order
            if cat_name not in SettingsDict["_order"]:
                logger.error("Category %s not included in _order" % cat_name)
                invalid_file = True

            # 4. All second-level dictionaries (variable names) contain datatype key
            #    If the variable is of a derived type, then datatype is a dictionary itself
            for var_name in SettingsDict[cat_name].keys():
                if "datatype" not in SettingsDict[cat_name][var_name].keys():
                    logger.error("Variable %s does not contain a key for datatype" % var_name)
                    invalid_file = True
                    continue

                # Rest of checks
                if not isinstance(SettingsDict[cat_name][var_name]["datatype"], dict):
                    if not _valid_variable_dict(SettingsDict[cat_name][var_name], var_name):
                        invalid_file = True
                else:
                    for subvar_name in SettingsDict[cat_name][var_name]["datatype"].keys():
                        if subvar_name[0] != '_':
                            if not _valid_variable_dict(SettingsDict[cat_name][var_name]["datatype"][subvar_name],
                                                        "%s%%%s"  % (var_name, subvar_name)):
                                invalid_file = True

    return (not invalid_file)

################################################################################

def diagnostics_dictionary_is_consistent(DiagsDict):
    """ Make sure dictionary generated from JSON settings file conforms to MARBL
        diagnostics file standards:
        1. DiagsDict must be a dictionary
        2. All diagnostic variable dictionaries contain the following keys:
           i.   longname
           ii.  units
           iii. vertical_grid (2D vars should explicitly list "none")
           iv.  frequency
           v.   operator
        3. Diagnostic variable dictionaries may contain 'dependencies' key as well,
           but it must be a dictionary itself.
        4. Consistency between frequency and operator
           i.   frequency and operator are both lists, or neither are
           ii.  If they are both lists, must be same size
        5. Allowable frequencies are never, low, medium, and high
        6. Allowable operators are instantaneous, average, minimum, and maximum
    """

    import logging
    logger = logging.getLogger(__name__)
    invalid_file = False

    if not isinstance(DiagsDict, dict):
        logger.error("Argument must be a dictionary")
        return False

    # Consistency checks
    for diag_name in DiagsDict.keys():
        # 1. diag_dict must be dictionary
        diag_dict = DiagsDict[diag_name]
        if not isinstance(diag_dict, dict):
            logger.error("DiagsDict['%s'] must be a dictionary" % diag_name)
            invalid_file = True
            continue

        # 2. diag_dict must have the following keys:
        valid_keys = ["longname", "units", "vertical_grid", "frequency", "operator"]
        for key_check in valid_keys:
            if key_check not in diag_dict.keys():
                message = "Diagnostic %s is not well-defined in YAML" % diag_name
                message = message + "\n     * Expecting %s as a key" % key_check
                logger.error(message)
                invalid_file = True
        # 3. If diag_dict has 'dependencies' key, diag_dict['dependencies'] must be a dictionary
        if 'dependencies' in diag_dict.keys():
            if not isinstance(diag_dict['dependencies'], dict):
                message = "Diagnostic %s is not well-defined in YAML" % diag_name
                message = message + "\n     * 'dependencies' is a key to non-dictionary entry"
                logger.error(message)
                invalid_file = True

        # 4. Consistency between frequency and operator
        err_prefix = "Inconsistency in DiagsDict['%s']:" % diag_name
        #    i.   frequency and operator are both lists, or neither are
        if isinstance(diag_dict['frequency'], list) != isinstance(diag_dict['operator'], list):
            logger.error("%s either both frequency and operator must be lists or neither can be" % err_prefix)
            invalid_file = True
            continue

        #    ii.  If they are both lists, must be same size
        if isinstance(diag_dict['frequency'], list):
            freq_len = len(diag_dict['frequency'])
            op_len = len(diag_dict['operator'])
            if freq_len != op_len:
                logger.error("%s frequency is length %d but operator is length %d" %
                             (err_prefix, diag_name, freq_len, op_len))
                invalid_file = True
                continue

        # 5. Allowable frequencies are never, low, medium, and high
        # 6. Allowable operators are instantaneous, average, minimum, and maximum
        ok_freqs = ['never', 'low', 'medium', 'high']
        ok_ops = ['instantaneous', 'average', 'minimum', 'maximum']
        invalid_freq_op = False
        if not isinstance(diag_dict['frequency'], dict):
            if isinstance(diag_dict['frequency'], list):
                for freq, op in zip(diag_dict['frequency'], diag_dict['operator']):
                    if freq not in ok_freqs:
                        logger.error("%s '%s' is not a valid frequency" % (err_prefix, freq))
                        invalid_freq_op = True
                    if op not in ok_ops:
                        logger.error("%s '%s' is not a valid operator" % (err_prefix, op))
                        invalid_freq_op = True
            else:
                freq = diag_dict['frequency']
                op = diag_dict['operator']
                if freq not in ok_freqs:
                    logger.error("%s '%s' is not a valid frequency" % (err_prefix, freq))
                    invalid_freq_op = True
                if op not in ok_ops:
                    logger.error("%s '%s' is not a valid operator" % (err_prefix, op))
                    invalid_freq_op = True
        if invalid_freq_op:
            invalid_file = True

    return (not invalid_file)

################################################################################
#                            PRIVATE MODULE METHODS                            #
################################################################################

def _valid_variable_dict(var_dict, var_name):
    """ Return False if dictionary does not contain any of the following:
        * longname
        * subcategory
        * units
        * datatype
        * default_value
    """

    import logging
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

def _valid_diagnostic_dict(diag_dict, diag_name, has_module=True, check_freq=True):
    """ Return False if any of the following:
        1. diag_dict is not a dictionary
        2. diag_dict does not contain any of the following:
           * longname
           * units
           * vertical_grid
           * frequency
           * operator
           Diagnostics that are not tracer-specific should also include
           * module
        3. Consistency between frequency and operator
        4. Allowable frequencies are never, low, medium, and high
        5. Allowable operators are instantaneous, average, minimum, and maximum

        TODO: parse frequency defaults when tracer-dependent and then remove check_freq flag
    """

    import logging
    logger = logging.getLogger(__name__)


################################################################################
