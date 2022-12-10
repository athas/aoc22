#!/usr/bin/env python3

import sys
import struct

v = sys.stdin.buffer.read()
sys.stdout.buffer.write(v[7:])
