from homework_planner.homework_planner import planning_dict, homework_this_week

a = planning_dict()

def test_01():
    assert a['Week 1']['Theme'] == 'Functional Programming'




def test_02():
    hw_w1 = homework_this_week(a['Week 1'])
    assert hw_w1[0] == 'ello'




# - read the book pages 1-31
# - watch lectures 1 & 2
# - read the course notes for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)
# - do homework for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)
#   - you will need to read [word.txt](reference/berkeley_cs61a_material/course_reader_vol_2/word.txt)
# - do labs for week 1 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)