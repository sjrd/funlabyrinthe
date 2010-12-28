# -*- coding: utf-8 -*-

import os, shutil
from ConfigParser import ConfigParser

def _getAppDataDir():
    config = ConfigParser()
    config.read('..\\FunLabyrinthe.ini')
    return config.get('Directories', 'AppData')

SRC_APPDATA = unicode(_getAppDataDir())
DST_APPDATA = unicode(os.path.join(os.getcwd(), 'AppData'))

MAIN_DIRS = (
    u'Labyrinths',
    u'Saveguards',
    u'Sounds',
    u'Squares',
    u'Units'
)

def cleanAppDataDir():
    """Clean the AppData\ install dir."""
    print u'Clean AppData\\'
    
    if not os.path.exists(DST_APPDATA):
        os.mkdir(DST_APPDATA)
    
    for dir in MAIN_DIRS:
        path = os.path.join(DST_APPDATA, dir)
        if os.path.exists(path):
            shutil.rmtree(path)

def getIgnorePatternsFor(dir):
    """Get the ignore patterns to apply for a given main dir."""
    with open(u'Ignore.%s.txt' % dir, 'r') as file:
        lines = file.readlines()
    
    patterns = []
    for line in lines:
        pattern = line.strip()
        if pattern and pattern[0] != '#':
            patterns.append(pattern.decode('utf-8'))
    
    return patterns

def fetchMainDir(dir):
    """Fetch a main dir."""
    print u'Populate AppData\\'+dir+u'\\'
    
    patterns = getIgnorePatternsFor(dir)
    ignore = shutil.ignore_patterns(*patterns)
    
    srcdir = os.path.join(SRC_APPDATA, dir)
    dstdir = os.path.join(DST_APPDATA, dir)
    
    shutil.copytree(srcdir, dstdir, False, ignore)

def fetchAllMainDirs():
    """Fetch all main dirs."""
    for dir in MAIN_DIRS:
        fetchMainDir(dir)

def main():
    """Main function"""
    cleanAppDataDir()
    fetchAllMainDirs()

if __name__ == '__main__':
    main()
