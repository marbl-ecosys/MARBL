""" Subroutines that are accessed from both MARBL_settings_file_class.py
    and MARBL_diagnostics_file_class.py
"""

################################################################################
#                            PUBLIC MODULE METHODS                             #
################################################################################

def abort(err_code=0):
    """ This routine imports sys and calls exit
    """
    import sys
    sys.exit(err_code)

################################################################################

def sort(list_in, sort_key=lambda s: s.lower()):
    """ Sort a list; default is alphabetical (case-insensitive), but that
        can be overridden with the sort_key argument
    """
    return sorted(list_in, key=sort_key)

################################################################################

def natural_sort_key(string_):
    """ From https://stackoverflow.com/questions/2545532/python-analog-of-natsort-function-sort-a-list-using-a-natural-order-algorithm/3033342#3033342
    """
    import re
    return [int(s) if s.isdigit() else s for s in re.split(r'(\d+)', string_)]

################################################################################

def expand_template_value(key_name, MARBL_settings, unprocessed_dict, check_freq=False):
    """ unprocessed_dict is a dictionary whose keys / values have templated strings (e.g.
        strings that depend on tracer name or PFT name). This subroutine replaces the
        templated values and adds the appropriate entry / entries to processed_dict

        check_freq option: if true, then if 'frequency' is a dictionary determine
        correct default value! (Only used for diagnostics, not for settings)
    """

    processed_dict = dict()

    import re
    import logging
    logger = logging.getLogger(__name__)

    template = re.search('\(\(.*\)\)', key_name).group()
    template_fill_dict = dict()
    if template == '((tracer_short_name))':
        fill_source = 'tracers'
        # diag name will replace template with key_fill_vals
        loop_for_replacement = MARBL_settings.tracers_dict.keys()
    elif template == '((autotroph_sname))':
        fill_source = 'autotrophs'
        loop_for_replacement = range(1,MARBL_settings.settings_dict['autotroph_cnt']+1)
    elif template == '((zooplankton_sname))':
        fill_source = 'zooplankton'
        loop_for_replacement = range(1,MARBL_settings.settings_dict['zooplankton_cnt']+1)
    elif template == '((particulate_flux_ref_depth_str))':
        fill_source = 'strings'
        particulate_flux_ref_depth_str = '%dm' % MARBL_settings.settings_dict['particulate_flux_ref_depth']
        loop_for_replacement = [ particulate_flux_ref_depth_str ]
    else:
        logger.error("%s is not a valid template value" % template)
        abort(1)

    # Loop over every tracer, autotroph, or zooplankton
    for item in loop_for_replacement:
        # i. populate template_fill_dict
        if fill_source == 'tracers':
            key_fill_val = item
            tracer_dict = MARBL_settings.tracers_dict[key_fill_val]
            # more metadata will be available in template_fill_dict
            template_fill_dict['((tracer_long_name))'] = tracer_dict["long_name"]
            template_fill_dict['((tracer_tend_units))'] = tracer_dict["tend_units"]
            del tracer_dict
            # Check to see if tracer is in tracer_restore_vars(:)
            template_fill_dict['((restore_this_tracer))'] = False
            for n in range(1,MARBL_settings.get_tracer_cnt()+1):
                if key_fill_val == MARBL_settings.settings_dict["tracer_restore_vars(%d)" % n].strip('"'):
                    template_fill_dict['((restore_this_tracer))'] = True
                    break
        elif fill_source == 'autotrophs':
            auto_prefix = "autotroph_settings(%d)%%" % item
            key_fill_val = MARBL_settings.settings_dict[auto_prefix + "sname"].strip('"')
            # Autotroph properties
            imp_calcifier = (MARBL_settings.settings_dict[auto_prefix + "imp_calcifier"].strip('"'))
            exp_calcifier = (MARBL_settings.settings_dict[auto_prefix + "exp_calcifier"].strip('"'))
            silicifier = (MARBL_settings.settings_dict[auto_prefix + "silicifier"].strip('"'))
            Nfixer = (MARBL_settings.settings_dict[auto_prefix + "Nfixer"].strip('"'))
            # Add values to template_fill_dict
            template_fill_dict['((autotroph_lname))'] = MARBL_settings.settings_dict[auto_prefix + "lname"].strip('"')
            template_fill_dict['((autotroph_calcifier))'] = ".true." in [imp_calcifier, exp_calcifier]
            template_fill_dict['((autotroph_silicifier))'] = (silicifier == ".true.")
            template_fill_dict['((autotroph_Nfixer))'] = (Nfixer == ".true.")
        elif fill_source == 'zooplankton':
            zoo_prefix = "zooplankton_settings(%d)%%" % item
            key_fill_val = MARBL_settings.settings_dict[zoo_prefix + "sname"].strip('"')
            template_fill_dict['((zooplankton_lname))'] = MARBL_settings.settings_dict[zoo_prefix + "lname"].strip('"')
        elif fill_source == 'strings':
            key_fill_val = item
            template_fill_dict[template] = item

        # ii. Determine name of new diagnostic
        new_key_name = key_name.replace(template, key_fill_val)
        remove_entry = False
        processed_dict[new_key_name] = dict()

        # iii. Loop over every key in the unprocessed diagnostic dictionary, replace templated values
        for key in unprocessed_dict.keys():
            # Keys that are dictionaries should be treated differently
            if not isinstance(unprocessed_dict[key], dict):
                # look for templates in values
                if isinstance(unprocessed_dict[key], type(u'')):
                    if re.search('\(\(.*\)\)', unprocessed_dict[key]) == None:
                        processed_dict[new_key_name][key] = unprocessed_dict[key]
                    else:
                        template2 = re.search('\(\(.*\)\)', unprocessed_dict[key]).group()
                        try:
                            replacement_text = template_fill_dict[template2]
                        except:
                            logger.error("Can not replace '%s'" % template2)
                            abort(1)
                        processed_dict[new_key_name][key] = unprocessed_dict[key].replace(template2, replacement_text)
                else:
                    processed_dict[new_key_name][key] = unprocessed_dict[key]
            else:
                # Only "dependencies" and "frequency" can be dictionaries
                if key == 'dependencies':
                    # need to check dependencies on a per-diagnostic basis
                    for dependency in unprocessed_dict['dependencies'].keys():
                        if dependency in template_fill_dict.keys():
                            check_val = template_fill_dict[dependency]
                        else:
                            try:
                                check_val = MARBL_settings.settings_dict[dependency]
                            except:
                                logger.error("Unknown dependency '%s'" % dependency)
                                abort(1)
                        if unprocessed_dict['dependencies'][dependency] != check_val:
                            remove_entry = True
                            break
                elif key == 'frequency':
                    dict_key = 'default'
                    for new_key in unprocessed_dict[key].keys():
                        #if new_key == '((restore_this_tracer))':
                        if new_key in template_fill_dict.keys():
                            if template_fill_dict[new_key]:
                                dict_key = new_key
                                break
                    processed_dict[new_key_name][key] = unprocessed_dict[key][dict_key]
                else:
                    logger.error("Not expecting '%s' key to be a dictionary" % key)
                    abort(1)

        # If dependencies prevent diagnostic from being used, remove it from processed_dict
        if remove_entry:
            del processed_dict[new_key_name]

    return processed_dict

################################################################################

def meet_dependencies(input_dict, MARBL_settings):
    if "dependencies" in input_dict.keys():
        for dependency in input_dict["dependencies"].keys():
            if dependency not in MARBL_settings.settings_dict.keys():
                logger.error("'%s' is not a valid dependency" % dependency)
                abort(1)
            if input_dict["dependencies"][dependency] != MARBL_settings.settings_dict[dependency]:
                return False
    return True
