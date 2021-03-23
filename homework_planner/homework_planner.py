import pprint
from pathlib import Path
from typing import List

import yaml


def planning_dict():
    a = Path('tasks.yaml')
    with a.open() as f:
        return yaml.safe_load(f.read())


def write_plan():
    pass

class Reference(object):
    def __init__(self, dictie):
        if 'Reading_extra' in dictie:
            self.title, self.path = dictie['Reading_extra']['title'], \
                          dictie['Reading_extra'][
                              'path']
    def url(self):
        return 'hello'

    @property
    def markdown_link(self):
        return f'[{self.title}]({self.path})'


def homework_this_week(value: dict) -> List[str]:
    week = value["week"]

    labs = "[](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)"
    # - read the course notes for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)
    # - do homework for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)
    #   - you will need to read [word.txt](reference/berkeley_cs61a_material/course_reader_vol_2/word.txt)
    # - do labs for week 1 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)


    if "book_exercises" in value:
        homework_line_extra = f' including book exercises {value["book_exercises"]}'
    else:
        homework_line_extra = ""

    return [f'do labs for week {week}',
            f'read the book {value["Reading"]}',
            f'watch the lectures {value["Lectures"]}',
            f'read the course notes for week {week}',
            f'do homework for week {week}{homework_line_extra}',
            f'cross-check your homework (solutions/week{week}.txt)'
            ]

def markdown_output():
    target = Path(__file__).parent/ 'homework.md'
    with target.open('w') as f:
        all_weeks = planning_dict()
        for week in all_weeks:
            f.write(f'# {week}\n')
            for h in homework_this_week(all_weeks[week]):
                f.write(f'- {h}\n')
            f.write('\n\n')



if __name__ == '__main__':
    markdown_output()