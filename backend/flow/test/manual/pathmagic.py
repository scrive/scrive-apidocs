import os
import sys
parent_dir = os.path.dirname(os.path.abspath(__file__))

root_path = os.path.join(parent_dir, '..')
sys.path.append(os.path.realpath(root_path))
