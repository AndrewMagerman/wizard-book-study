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


def homework_this_week(value: dict) -> List[str]:
    week = value["week"]

    if "book_exercises" in value:
        homework_line_extra = f' including book exercises {value["book_exercises"]}'
    else:
        homework_line_extra = ""

    return [f'do labs for week {week}',
            f'read the book {value["Reading"]}',
            f'watch the lectures {value["Lectures"]}',
            f'read the course notes for week {week}',
            f'do homework for week {week}{homework_line_extra}'
            ]


def mickey():
    all_weeks = planning_dict()
    for week in all_weeks:
        print(week)
        print('\n')
        for h in homework_this_week(all_weeks[week]):
            print(h)
        print('\n')

if __name__ == '__main__':
    pprint.pprint(planning_dict())
    #a = planning_dict()
    #print(homework_this_week(a['Week 2']))
    mickey()