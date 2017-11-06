#!/usr/bin/env python

""" CIME, as found in CESM 2.0, does not support python libraries beyond the
standard libraries... so PyYAML is not necessarily available on CESM-supported
machines. Until CESM requires YAML support, we are offering this tool to
convert YAML -> JSON, and CESM will read the JSON configuration file.
"""

import yaml, json
yaml_file = "../../src/default_settings.yaml"
json_file = "default_settings.json"
with open(yaml_file) as file_in:
  with open(json_file, "w") as file_out:
    json.dump(yaml.safe_load(file_in), file_out, separators=(',', ': '), sort_keys=True, indent=3)
  
