# Configuration for IPython

c = c  # noqa: 'c' is defined but flake8 doesn't know about it

c.TerminalIPythonApp.display_banner = False

c.InteractiveShell.autocall = 1  # smart
c.InteractiveShell.show_rewritten_input = True
# c.InteractiveShell.automagic = True
# c.InteractiveShell.separate_in = '\n'
# c.InteractiveShell.pdb = False

c.TerminalInteractiveShell.confirm_exit = False

c.PromptManager.in_template = '\\# >>> '
c.PromptManager.in2_template = '\\# ... '
c.PromptManager.out_template = '\\#: '

# c.IPCompleter.limit_to__all__ = False
