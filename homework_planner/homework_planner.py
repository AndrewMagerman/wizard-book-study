from pathlib import Path
from typing import List

import yaml


def planning_dict():
    a = Path('tasks.yaml')
    with a.open() as f:
        return yaml.safe_load(f.read())


def write_plan():
    pass


def homework_this_week(param: dict) -> List[str]:
    return ['hello']


if __name__ == '__main__':
    print(planning_dict())