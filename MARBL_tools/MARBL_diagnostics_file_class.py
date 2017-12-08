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

        # 3. Dictionary for keeping diagnostic, frequency pairs
        from collections import OrderedDict
        self.diagnostics_dict = OrderedDict()
        for diag_name in _sort(self._diagnostics.keys()):
            self._process_diagnostic_frequency(diag_name)

        # 5.

    ################################################################################
    #                            PRIVATE CLASS METHODS                             #
    ################################################################################

    def _process_diagnostic_frequency(self, diag_name):
        if isinstance(self._diagnostics[diag_name]['frequency'], list):
            self.diagnostics_dict[diag_name] = ", ".join(self._diagnostics[diag_name]['frequency'])
        else:
            self.diagnostics_dict[diag_name] = self._diagnostics[diag_name]['frequency']

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