from pathlib import Path

from homework_planner.homework_planner import planning_dict, homework_this_week, \
    Reference

all_weeks = planning_dict()


def test_01():
    assert all_weeks['Week 1']['Theme'] == 'Functional Programming'


def test_02():
    hw_w1 = homework_this_week(all_weeks['Week 1'])
    print(hw_w1)
    assert hw_w1[0] == "do labs for week 1"
    assert hw_w1[1] == "read the book Section 1.1, pages 1-31"
    assert hw_w1[2] == "do homework for week 1"


def test_week_2():
    hw_w2 = homework_this_week(all_weeks['Week 2'])
    print(all_weeks['Week 2'])
    assert hw_w2[
               2] == "do homework for week 2 including book exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46"


def test_reading_extra():
    b = all_weeks['Week 7']
    title, path = b['Reading_extra']['title'], b['Reading_extra']['path']
    w = Path(__file__).parent.parent
    print(w)
    c = w / path
    print(c.absolute())
    assert Path(c) == 'hello'


def test_file_exists():
    for week, dictie in all_weeks.items():
        if 'Reading_extra' in dictie:
            title, path = dictie['Reading_extra']['title'], \
                          dictie['Reading_extra'][
                              'path']
            w = Path(__file__).parent.parent
            c = w / path
            assert c.exists()


def test_reference():
    k = Reference({'Reading_extra': {'title': 'text',
                                     'path': 'http://a.com'}})

    assert k.markdown_link == '[text](http://a.com)'

    l = Reference({'Reading_extra': {'title': 'lab',
                                     'path': 'reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf'}})

    assert l.markdown_link == '[lab](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)'
# - read the book pages 1-31
# - watch lectures 1 & 2
# - read the course notes for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)
# - do homework for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)
#   - you will need to read [word.txt](reference/berkeley_cs61a_material/course_reader_vol_2/word.txt)
# - do labs for week 1 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
