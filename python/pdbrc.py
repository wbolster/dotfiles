import atexit
import pathlib
import readline

# preserve readline history between sessions
readline.set_history_length(10000)
history_path = pathlib.Path("~/.pdb_history").expanduser()
if history_path.exists():
    readline.read_history_file(history_path)
atexit.register(readline.write_history_file, history_path)
