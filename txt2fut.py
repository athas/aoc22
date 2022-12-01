#!/usr/bin/env python

import sys
import struct

v = sys.stdin.buffer.read()
sys.stdout.buffer.write(b'b')
sys.stdout.buffer.write(b'\2')
sys.stdout.buffer.write(b'\1')
sys.stdout.buffer.write(b'  u8')
sys.stdout.buffer.write(struct.pack('<Q', len(v)))
sys.stdout.buffer.write(v)
