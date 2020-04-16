import unittest
import datetime
import logging
import sys
import wasp
import constants as co



class WaspSetTestCase(unittest.TestCase):

    def setUp(self):
        self.hydfile = '/tmp/test.HYD'
        self.dlevel = 1
        self.dt = datetime.datetime(2019, 1, 1, 0, 0, 0)
        self.desc = [
            'Description 1',
            'Description 2',
            'Description 3'
        ]
        self.author = 'Ingesur SRL'

    def tearDown(self):
        # self.hydfile.dispose()
        pass

    def test_debug(self):
        w = wasp.Wasp()
        w.setDebug(self.dlevel)

    def test_open(self):
        w = wasp.Wasp()
        w.setDebug(self.dlevel)
        w.open(self.hydfile, co.OPEN_WRITE)
        w.close()

    def test_run2D(self):
        log = logging.getLogger()
        NOSEG = 1000
        NUMFLOW = 1200
        NUMDHT = 1
        HDT = 1
        START = 1
        END = 1
        NUM_LAYER = 1
        w = wasp.Wasp()
        w.setDebug(self.dlevel)
        # Default write
        w.open(self.hydfile, co.OPEN_WRITE)
        w.setLang(co.LANG_C)
        w.setDescriptions(self.desc)
        w.setAuthor(self.author)
        w.setMoment(self.dt)
        w.setNumLay(NUM_LAYER)
        w.setNumSeg(NOSEG)
        ss = list()
        for i in range(NOSEG):
            s = str(i+1).zfill(7)
            ss.append('WASPS-{}'.format(s))
        w.setSegNames(ss)
        # Get compact factor
        w.compactFactor()
        log.debug('**** Compact factor: {}'.format(w.cf))
        w.close()

    def test_run3D(self):
        pass

logging.basicConfig(
    filename='tests_wasp.log',
    filemode='w',
    level=logging.DEBUG,
    format='%(name)s - %(levelname)s - %(message)s'
)

if __name__ == '__main__':
    unittest.main()
