#!/usr/bin/env python3

from __future__ import print_function, absolute_import, division

import logging
import os

from errno import EACCES
from os.path import realpath
from threading import Lock

from fusepy import FUSE, FuseOSError, Operations, LoggingMixIn, fuse_get_context


class Loopback(LoggingMixIn, Operations):
    def __init__(self):
        self.rwlock = Lock()

    def __get_root(self):
        uid, gid, pid = fuse_get_context()
        print(str(pid))
        environ = open('/proc/' + str(pid) + '/environ').read()
        idx = environ.find('WORKENVDIR=')
        print(idx)
        if idx == -1:
            return '/home/yhryhorenko/.empty/'
        else:
            idx2 = environ.find('\000', idx + 1)
            print(environ[idx + 11:idx2])
            return environ[idx + 11:idx2]
        
    def __call__(self, op, path, *args):
        root = self.__get_root()
        return super(Loopback, self).__call__(op, root + path, *args)

    def access(self, path, mode):
        if not os.access(path, mode):
            raise FuseOSError(EACCES)

    chmod = os.chmod
    chown = os.chown

    def create(self, path, mode):
        return os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)

    def flush(self, path, fh):
        return os.fsync(fh)

    def fsync(self, path, datasync, fh):
        if datasync != 0:
            return os.fdatasync(fh)
        else:
            return os.fsync(fh)

    def getattr(self, path, fh=None):
        st = os.lstat(path)
        return dict((key, getattr(st, key)) for key in (
            'st_atime', 'st_ctime', 'st_gid', 'st_mode', 'st_mtime',
            'st_nlink', 'st_size', 'st_uid'))

    getxattr = None

    def link(self, target, source):
        root = self.__get_root()
        return os.link(root + source, target)

    listxattr = None
    mkdir = os.mkdir
    mknod = os.mknod
    open = os.open

    def read(self, path, size, offset, fh):
        with self.rwlock:
            os.lseek(fh, offset, 0)
            return os.read(fh, size)

    def readdir(self, path, fh):
        return ['.', '..'] + os.listdir(path)

    readlink = os.readlink

    def release(self, path, fh):
        return os.close(fh)

    def rename(self, old, new):
        return os.rename(old, self.__get_root() + new)

    rmdir = os.rmdir

    def statfs(self, path):
        stv = os.statvfs(path)
        return dict((key, getattr(stv, key)) for key in (
            'f_bavail', 'f_bfree', 'f_blocks', 'f_bsize', 'f_favail',
            'f_ffree', 'f_files', 'f_flag', 'f_frsize', 'f_namemax'))

    def symlink(self, target, source):
        return os.symlink(source, target)

    def truncate(self, path, length, fh=None):
        with open(path, 'r+') as f:
            f.truncate(length)

    unlink = os.unlink
    utimens = os.utime

    def write(self, path, data, offset, fh):
        with self.rwlock:
            os.lseek(fh, offset, 0)
            return os.write(fh, data)


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    # parser.add_argument('root')
    parser.add_argument('mount')
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG)
    fuse = FUSE(Loopback(), args.mount, foreground=True)
