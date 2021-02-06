from IPython.terminal.prompts import ClassicPrompts
from pygments.token import Token

c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.highlighting_style = 'monokai'
c.TerminalInteractiveShell.highlighting_style_overrides = {Token.Prompt: '#888'}
c.TerminalInteractiveShell.prompts_class = ClassicPrompts
c.TerminalInteractiveShell.separate_in = ''
