import unittest
import datetime
import logging
import sys
import wasp


class WaspGetTestCase(unittest.TestCase):

    def setUp(self):
        self.hydfile = 'data_wasp17/WASP8.HYD'
        self.dlevel = 1

    def tearDown(self):
        # self.hydfile.dispose()
        pass

"""
    def test_debug(self):
        w = wasp.Wasp()
        w.setDebug(self.dlevel)

    def test_open_close(self):
        w = wasp.Wasp()
        w.setDebug(self.dlevel)
        w.open(self.hydfile, wasp.OPEN_READ)
        w.close()

    def test_descriptions(self):
        pass

    def test_author(self):
        pass

    def test_numseg(self):
        pass

    def test_segnames(self):
        pass

    def test_numfp(self):
        pass
"""

logging.basicConfig(
    filename='tests_wasp.log',
    filemode='w',
    level=logging.DEBUG,
    format='%(name)s - %(levelname)s - %(message)s'
)

if __name__ == '__main__':
    unittest.main()
