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


def homework_this_week(value: dict, target='markdown') -> List[str]:
    week = value["week"]

    labs, course_notes, homework = 'labs', 'course_notes', 'homework'

    list1 = []

    if target == 'markdown':
        labs = "[labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)"
        course_notes = "[course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)"
        homework = "[homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)"

    if "book_exercises" in value:
        homework_line_extra = f' including book exercises {value["book_exercises"]}'
    else:
        homework_line_extra = ""

    if "lab_exercises" in value:
        lab_line_extra = f' including book exercises {value["lab_exercises"]}'
    else:
        lab_line_extra = ""

    if "nolabs" not in value:
        list1.extend([f'do {labs} for week {week}{lab_line_extra}',
                      ]
                     )

    if "Reading" in value:
        if value["Reading"] != "":
            list1.extend([f'read the book {value["Reading"]}'])

    if "Reading_extra" in value:
        title = value["Reading_extra"]["title"]
        print(title)
        path = value["Reading_extra"]["path"]
        list1.extend([f'read [{title}]({path})'])

    if "Lectures" in value:
        list1.extend([f'watch the lectures {value["Lectures"]}'])

    if "video" in value:
        list1.extend([f'video link : {value["video"]}'])

    list1.extend([
        f'read the {course_notes} for week {week}',
        f'do {homework} for week {week}{homework_line_extra}',
    ])

    if "solutions" in value:
        solution = value["solutions"]
        list1.extend([
            f'cross-check your homework ({solution})',
        ])

    if "project" in value:
        list1.extend([f'do {value["project"]}'])

    if "exams" in value:
        list1.extend([f'do {value["exams"]}'])

    return list1


def date_generator():
    start_date = datetime.datetime(2021, 3, 24, 18, 30)

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
            f.write(
                f'\n\nWe **review** this work on Review Meeting {week} on {all_weeks[week]["Review_Meeting"]}')
            f.write('\n\n')


def discord_output():
    target = Path(__file__).parent / 'homework_discord.md'
    with target.open('w') as f:

        all_weeks = planning_dict()
        for week in all_weeks:
            f.write(f'{week} ({all_weeks[week]["Theme"]})\n\n')
            for h in homework_this_week(all_weeks[week], target='discord'):
                f.write(f'- {h}\n')
            f.write(
                f'\n\nWe **review** this work on Review Meeting {week} on {all_weeks[week]["Review_Meeting"]}')
            f.write('\n\n')
            link_summary(f)
            f.write('\n\n')


def link_summary(f):
    f.write('repo: https://github.com/AndrewMagerman/wizard-book-study\n')
    base = "https://github.com/AndrewMagerman/wizard-book-study/tree/main/"
    f.write(
        f'labs = {base}reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf\n')
    f.write(
        f'course_notes = {base}reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf\n')
    f.write(
        f'homework = {base}reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf\n\n')


if __name__ == '__main__':
    discord_output()
    markdown_output()
