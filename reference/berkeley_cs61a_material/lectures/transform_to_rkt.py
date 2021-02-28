import glob
from pathlib import Path

def all_scm_files():
    return glob.glob("**/*.scm", recursive=True)

def change_suffix(file: Path):
    Path.rename(file, file.with_suffix('.rkt'))
    pass

if __name__ == '__main__':

    # change_suffix(Path(r'/Users/andrewmagerman/sourcecontrol/wizard-book-study/reference/berkeley_cs61a_material/lectures/mapreduce-demo.scm'))

    for p in all_scm_files():
        change_suffix(Path(p))