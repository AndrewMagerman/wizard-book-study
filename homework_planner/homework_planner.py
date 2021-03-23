import datetime
from pathlib import Path
from typing import List

import pytz
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

    labs = "[labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)"
    course_notes = "[course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)"
    homework = "[homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)"

    if "book_exercises" in value:
        homework_line_extra = f' including book exercises {value["book_exercises"]}'
    else:
        homework_line_extra = ""

    list1 = [f'do {labs} for week {week}',
             ]

    if "Reading" in value:
        if value["Reading"] != "":
            list1.extend([f'read the book {value["Reading"]}'])

    if "Reading_extra" in value:
        title = value["Reading_extra"]["title"]
        print(title)
        path = value["Reading_extra"]["path"]
        list1.extend([f'read [{title}]({path})'])

    list1.extend([f'watch the lectures {value["Lectures"]}',
                  f'read the {course_notes} for week {week}',
                  f'do {homework} for week {week}{homework_line_extra}',
                  f'cross-check your homework (solutions/week{week}.txt)',
                  ])

    if "project" in value:
        list1.extend([f'do {value["project"]}'])

    if "exams" in value:
        list1.extend([f'do {value["exams"]}'])

    return list1

def date_generator():
    start_date =  datetime.datetime(2021, 3, 24, 18, 30)

    timezone = pytz.timezone("Europe/Zurich")
    d_aware = timezone.localize(start_date)
    yield d_aware
    while True:
        d_aware += datetime.timedelta(weeks=2)
        yield d_aware

def markdown_output():
    dg = date_generator()
    target = Path(__file__).parent / 'homework.md'
    with target.open('w') as f:
        all_weeks = planning_dict()
        for week in all_weeks:
            f.write(f'## {week} ({all_weeks[week]["Theme"]})\n\n')
            for h in homework_this_week(all_weeks[week]):
                print(h)
                f.write(f'- {h}\n')
            d = next(dg)
            f.write(f'\n\nWe **review** this work on Meeting {week}\n on {d.strftime("%c %Z")}')
            f.write('\n\n')


if __name__ == '__main__':
    markdown_output()
    #print(date_generator().strftime('%c %Z'))
