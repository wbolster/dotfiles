# Configuration for IPython
# This file can be symlinked from /.ipython/profile_default

import os
import sys

import IPython
from IPython.terminal.prompts import Prompts, Token


class MyPrompts(Prompts):
    def in_prompt_tokens(self, cli=None):
        return [
            # (Token.PromptNum, '{:d} '.format(self.shell.execution_count)),
            (Token.Prompt, '>>> '),
        ]

    def continuation_prompt_tokens(self, cli=None, width=None):
        return [(Token.Prompt, '... '.rjust(width))]

    def out_prompt_tokens(self, cli=None):
        return []


c = c  # noqa: F821: 'c' is defined but flake8 doesn't know about it

c.InteractiveShell.separate_in = '\n'
c.InteractiveShell.prompts_class = MyPrompts

c.TerminalIPythonApp.display_banner = True
python_version = '.'.join(map(str, sys.version_info[:3]))
ipython_version = '.'.join(map(str, IPython.version_info[:3]))
sys_prefix = sys.prefix
home_dir = os.path.expanduser('~')
if sys_prefix.startswith(home_dir):
    sys_prefix = '~' + sys_prefix[len(home_dir):]
c.InteractiveShell.banner1 = "[Python {}, IPython {}, sys.prefix {}]\n".format(
    python_version, ipython_version, sys_prefix)
