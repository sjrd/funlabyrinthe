# -*- coding: utf-8 -*-

import os, shutil
from collections import namedtuple

syspath = os.getenv('PATH').split(os.path.pathsep)
syspath.append('C:\\windows\\SysWOW64')

RUNTIME_DIR = os.path.join(os.getcwd(), 'Runtime')

BDSInfo = namedtuple('BDSInfo',
    'numversion strversion')
BDS = BDSInfo(numversion = 120, strversion = '2009')

def cleanRuntimeDir():
    """Clean the Runtime\ install dir."""
    print 'Clean Runtime\\'

    files = os.listdir(RUNTIME_DIR)
    for file in files:
        filename = os.path.join(RUNTIME_DIR, file)
        os.remove(filename)

def findRuntimeFile(name):
    """Find a runtime file in the system path."""
    if os.path.exists(name):
        return name
    
    for dir in syspath:
        filename = os.path.join(dir, name)
        if os.path.exists(filename):
            return filename
    
    raise IOError('Cannot find file %s' % name)

def fetchRuntimeFile(name):
    """Fetch a given runtime file and store it in the Runtime\ install dir."""
    source = findRuntimeFile(name)
    dest = os.path.join(RUNTIME_DIR, name)
    
    shutil.copyfile(source, dest)

def fetchNeededRuntimeFiles():
    """Fetch all needed runtime files."""
    print 'Populate Runtime\\'

    with open('RuntimeFiles.txt', 'r') as file:
        lines = file.readlines()
    
    config = {'suffix': ''}
    globs = {'bds': BDS}
    
    for line in lines:
        line = line.strip()
        
        if not line or line[0] == '#':
            continue
        
        if line[0] == '!':
            # Command
            exec line[1:].lstrip() in globs, config
        else:
            # Regular file to fetch
            name = line + config['suffix']
            fetchRuntimeFile(name)

def main():
    """Main function"""
    cleanRuntimeDir()
    fetchNeededRuntimeFiles()

if __name__ == '__main__':
    main()
