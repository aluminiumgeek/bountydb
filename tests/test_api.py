#!/usr/bin/env python
#
# Test REST API

import httplib
import urllib
import unittest
import json
import time


class TestApi(unittest.TestCase):
    def setUp(self):
        self.host = 'localhost:3334'
        self.connection = httplib.HTTPConnection(self.host)
        
        self.key = '123testkey'
        self.value = 'this is value'
        self.default = 'this'
        self.timeout = 2
    
    def tearDown(self):
        self.connection.request('DELETE', '/store/{0}'.format(self.key))
        self.connection.close()
    
    def test_get_value(self):
        body = {
            'value': self.value
        }
        self._get_response('PUT', '/store/{0}'.format(self.key), body)
        
        response = self._get_response('GET', '/store/{0}'.format(self.key))
        self.assertEqual(response['status'], 'ok')
        self.assertEqual(response['value'], self.value)
    
    def test_get_default_value(self):
        url = 'http://{0}/store/{1}?default={2}'.format(
            self.host, 
            self.key, 
            self.default
        )
        response = json.loads(urllib.urlopen(url).read())
        
        self.assertEqual(response['status'], 'ok')
        self.assertEqual(response['value'], self.default)
    
    def test_get_no_value(self):
        response = self._get_response('GET', '/store/{0}'.format(self.key))
        self.assertEqual(response['status'], 'error')
        
    def test_put_value(self):
        body = {
            'value': self.value
        }
        
        response = self._get_response('PUT', '/store/{0}'.format(self.key), body)
        
        self.assertEqual(response['status'], 'ok')
        
    def test_put_value_with_timeout(self):
        body = {
            'value': self.value,
            'timeout': self.timeout
        }
        
        response = self._get_response('PUT', '/store/{0}'.format(self.key), body)
        
        self.assertEqual(response['status'], 'ok')
        
        time.sleep(self.timeout + 1)
        
        response = self._get_response('GET', '/store/{0}'.format(self.key))
        
        self.assertEqual(response['status'], 'error')
        
    def test_del_value(self):
        response = self._get_response('DELETE', '/store/{0}'.format(self.key))
        self.assertEqual(response['status'], 'ok')

    def _get_response(self, method, uri, body=''):
        self.connection.request(method, uri, json.dumps(body))
        
        result = self.connection.getresponse()
        
        return json.loads(result.read())


if __name__ == '__main__':
    unittest.main()