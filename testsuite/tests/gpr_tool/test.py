import os
from testsuite_support.utils import run_tool

# If GPR_TOOL is not defined, we should get the default value: startup-gen
run_tool(['-P', 'spec.gpr'])

# Define GPR_TOOL in the environment variables
os.environ["GPR_TOOL"] = "defined_in_env"

# If GPR_TOOL is defined on the command line it should have priority over
# environment variable and default value.
run_tool(['-P', 'spec.gpr', '-XGPR_TOOL=defined_on_cmd_line'])

# If GPR_TOOL is defined in environment variable, but not on the command line
# it should have priority over default value.
run_tool(['-P', 'spec.gpr'])
