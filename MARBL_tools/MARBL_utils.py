#!/usr/bin/env python

""" General tools that are used by multiple python files in this package
"""

################################################################################
#                            PUBLIC MODULE METHODS                             #
################################################################################

def settings_file_is_consistent(YAMLdict):
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

    import logging
    logger = logging.getLogger(__name__)
    invalid_file = False

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
