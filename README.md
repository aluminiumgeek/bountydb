#Bounty DB#

A simple key-value store written in Erlang.

Bounty DB is very fast and clean, it implements [Bloom filter](http://en.wikipedia.org/wiki/Bloom_filter‎) to reduce disk lookups for non-existent keys.

###Requirements###
* Erlang

###Setup###

* Run <code>make</code> inside the directory that contains cloned repo
* Open <code>main.config</code>, set the server port and path to db file
* Start server with <code>config=main ./start</code> command. However, you can make your config file and specify its filename to the <code>config</code> param.
* You can check status of the server with <code>./status</code> command, and stop the server by <code>./stop</code>

###API###
Bounty DB provides simple HTTP REST API.

####Get value####

    GET /store/{key}

You can append parameter <code>default={defaultValue}</code> to the request. This default value will return if there's no stored value for specified key in the database

Answer:

    {
        "status": "ok",
        "value": {value}
    }
When no value:

    {
        "status": "error"
    }

####Save value####

    PUT /store/{key}

Request body must be JSON string.  
Example, set value:

    {
        "value": "myValue"
    }

Example, set value with a *one minute timeout* on key (after this timeout has expired, the key will automatically deleted):

    {
        "value": "myValue",
        "timeout": 60
    }


Answer:

    {
        "status": "ok"
    }


####Delete value####

    DELETE /store/{key}

Answer:

    {
        "status": "ok"
    }
