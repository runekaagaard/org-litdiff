#!/usr/bin/env python
from sys import argv, exit
import re
from subprocess import check_output

CMD_GEN_TAGS = ("""{ctags_dir}/ctags --fields=* -o - {file_path} """
                """ | grep -e '!_TAG' -e "end:" > {tags_path}""")
CMD_FIND_TAG_BY_NAME = ("""{ctags_dir}/readtags -e -n -t {tags_path} """
                        """ -Q '(and (eq? $name "{name}"))' -l""")
CMD_FIND_TAG_BY_NAME_AND_SCOPE = (
    """{ctags_dir}/readtags -e -n -t {tags_path}"""
    """ -Q '(and (eq? $name "{name}") (eq? $scope-name "{scope_name}"))' -l""")
CMD_FIND_TAG_BY_LINE = ("{ctags_dir}/readtags -e -n -t {tags_path}"
                        " -Q '(and (<= $line {line}) (>= $end {line}))' -l")
CMD_PRINT_LINE_RANGE = "sed -n 1025,1029p"


def _call(cmd, *args):
    data = dict(zip(args[0].split(','), args[1:]))
    return check_output(cmd.format(**data), shell=True)


def _find_field(line, name):
    m = re.search(r'{}:([^\t]+)'.format(name), line)
    assert m
    return m.group(1)


def generate_tags(ctags_dir, tags_path, file_path):
    output = _call(CMD_GEN_TAGS, 'ctags_dir,tags_path,file_path', ctags_dir,
                   tags_path, file_path)
    print "OK"
    exit(0)


def find_tag_by_name(ctags_dir, tags_path, name, scope_name):
    cmd = (CMD_FIND_TAG_BY_NAME
           if scope_name == "0" else CMD_FIND_TAG_BY_NAME_AND_SCOPE)
    output = _call(cmd, 'ctags_dir,tags_path,name,scope_name', ctags_dir,
                   tags_path, name, scope_name)
    lines = output.strip().split("\n")
    assert len(lines) == 1
    line = lines[0]
    assert line.strip()
    line, end = _find_field(line, 'line'), _find_field(lines[0], 'end')
    print line, end
    exit(0)


def find_tag_by_line(ctags_dir, tags_path, line):
    output = _call(CMD_FIND_TAG_BY_LINE, 'ctags_dir,tags_path,line', ctags_dir,
                   tags_path, line)

    lines = output.strip().split("\n")
    assert len(lines) > 0
    line = lines[-1].strip()
    assert line
    name = line.split("\t")[0]
    try:
        scope = ":".join(_find_field(line, 'scope').split(":")[1:])
    except AssertionError:
        scope = None
    print name, scope
    exit(0)


locals()[argv[1]](*argv[2:])
