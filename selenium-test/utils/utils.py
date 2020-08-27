import contextlib
import os
import shutil
import tempfile


@contextlib.contextmanager
def temp_file_path():
    fd, file_path = tempfile.mkstemp()
    try:
        os.close(fd)
        yield file_path
    finally:
        try:
            os.remove(file_path)
        except OSError:
            pass


@contextlib.contextmanager
def temporary_dir():
    dir_path = tempfile.mkdtemp()
    try:
        yield dir_path
    finally:
        try:
            shutil.rmtree(dir_path)
        except OSError:
            pass


@contextlib.contextmanager
def change_work_dir(path):
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)


def create_empty_dir(dest_dir):
    if os.path.exists(dest_dir):
        shutil.rmtree(dest_dir)
    os.makedirs(dest_dir)
