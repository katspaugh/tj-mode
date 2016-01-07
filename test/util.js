'use strict';

var fs = require('fs');
var path = require('path');
var tern = require('tern');
var assert = require('assert');
var highlight = require('../highlight');

var ternDir = path.resolve(__dirname + '/../node_modules/tern');

var browser = require(ternDir + '/defs/browser.json');
var ecma5 = require(ternDir + '/defs/ecma5.json');

var allDefs = {
    browser: browser,
    ecma5: ecma5
};

var createServer = exports.createServer = function (defNames, plugins, options) {
    var defs = [];
    if (defNames) {
        for (var i = 0; i < defNames.length; i++) {
            var def = allDefs[defNames[i]];
            defs.push(def);
        }
    }
    if (!plugins) plugins = {};
    plugins['highlight'] = options ? options : {};

    highlight.initialize(ternDir);

    var server = new tern.Server({
        plugins: plugins,
        defs: defs
    });
    return server;
};

var assertHighlightReponse = exports.assertHighlightReponse = function (err, resp, expected) {
    if (err)
        throw err;
    var actualMessages = resp.messages;
    var expectedMessages = expected.messages;
    assert.equal(JSON.stringify(resp), JSON.stringify(expected));
};

exports.assertHighlight = function (text, expected, defNames, plugins, options) {
    var server = createServer(defNames, plugins, options);
    server.request({
        query: {
            type: 'highlight',
            text: text
        }
    }, function (err, resp) {
        assertHighlightReponse(err, resp, expected);
    });
};
