import logging
import MARBL_tools

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
        if not MARBL_tools.diagnostics_dictionary_is_consistent(self._diagnostics):
            logger.error("%s is not a valid MARBL diagnostics file" % default_diagnostics_file)
            MARBL_tools.abort(1)

        # 3. Generate processed version of self._diagnostics
        self.diagnostics_dict = dict()

        #    i. Expand per-tracer / per-PFT diagnostics
        #       [Look for keywords in (()) to signify need for string replacement]
        #       Note: use regular expressions to find substrings wrapped in '(())'
        import re
        for diag_name in self._diagnostics.keys():
            if re.search('\(\(.*\)\)', diag_name) == None:
                self.diagnostics_dict[diag_name] = dict(self._diagnostics[diag_name])
            else:
                # (Also determine correct frequency if 'frequency' is a dict)
                self.diagnostics_dict.update(MARBL_tools.expand_template_value(diag_name, MARBL_settings, self._diagnostics[diag_name], check_freq=True))

        #    ii. Delete diagnostics where dependencies are not met
        #        (Some diagnostics have already been removed via expand_template_value())
        for diag_name in self.diagnostics_dict.keys():
            if not MARBL_tools.meet_dependencies(self.diagnostics_dict[diag_name], MARBL_settings):
                del self.diagnostics_dict[diag_name]
                continue

            #    iii. frequency and operator should always be lists
            if not isinstance(self.diagnostics_dict[diag_name]['frequency'], list):
                self.diagnostics_dict[diag_name]['frequency'] = [self.diagnostics_dict[diag_name]['frequency']]
                self.diagnostics_dict[diag_name]['operator'] = [self.diagnostics_dict[diag_name]['operator']]
