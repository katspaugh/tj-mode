/**
 * @see https://github.com/katspaugh/tj-mode
 */

'use strict';


(function () {
    var initialize = function (tern, acorn, walk) {
        function addMessage(messages, node, type) {
            if (!node) { return; }

            var payload = {
                type: String(type || node.type),
                start: Number(node.start),
                end: Number(node.end)
            };

            if (node.message) {
                payload.message = String(node.message);
            }

            messages.push(payload);
        }

        function addError(messages, err) {
            addMessage(messages, {
                type: 'Error',
                start: err.pos || 0,
                end: err.raisedAt || 0,
                message: err.message
            });
        }

        function makeVisitors (server, query, collect) {
            return {
                VariableDeclaration: function (node) {
                    for (var i = 0, len = node.declarations.length; i < len; i++) {
                        var decl = node.declarations[i];

                        if (decl.init && decl.init.type === 'FunctionExpression') {
                            collect(decl.id, 'FunctionDeclaration');
                        } else {
                            collect(decl.id, 'VariableDeclaration');
                        }
                    }
                },

                Function: function (node) {
                    for (var i = 0, len = node.params.length; i < len; i++) {
                        collect(node.params[i], 'ArgumentDeclaration');
                    }

                    collect(node.id, 'FunctionDeclaration');
                },

                BlockStatement: function (node) {
                    collect(node);
                },

                ThisExpression: function (node) {
                    collect(node);
                },

                Property: function (node) {
                    if (node.value && node.value.type === 'FunctionExpression') {
                        collect(node.key, 'FunctionDeclaration');
                    }
                },

                Literal: function (node) {
                    if (typeof node.value === 'number') {
                        collect(node, 'NumberLiteral');
                    } else if (typeof node.value === 'string') {
                        collect(node, 'StringLiteral');
                    } else {
                        collect(node);
                    }
                }
            };
        }

        tern.defineQueryType('highlight', {
            run: function (server, query) {
                var messages = [];

                var collect = function (node, type) {
                    addMessage(messages, node, type);
                };

                // Parse to collect keyword tokens, comments, warnings and errors
                try {
                    var ast = acorn.parse(query.text, {
                        ecmaVersion: 6,

                        forbidReserved: 'everywhere',

                        onToken: function (token) {
                            token.type.keyword && collect({
                                type: 'Keyword',
                                start: token.start,
                                end: token.end
                            });
                        },

                        onComment: function (isBlock, text, start, end) {
                            collect({
                                type: 'Comment',
                                start: start,
                                end: end
                            });
                        },

                        onInsertedSemicolon: function (pos) {
                            collect({
                                type: 'Warning',
                                start: Math.max(0, pos - 1),
                                end: pos,
                                message: 'Missing semicolon'
                            });
                        },

                        onTrailingComma: function (pos) {
                            collect({
                                type: 'Warning',
                                start: pos,
                                end: pos + 1,
                                message: 'Trailing comma'
                            });
                        }
                    });

                    // Walk the error-tolerant parser AST
                    var visitors = makeVisitors(server, query, collect);
                    walk.simple(ast, visitors);
                } catch (err) {
                    addError(messages, err);
                }

                // Reply with a JSON object
                return {
                    highlight: messages
                };
            }
        });

        tern.registerPlugin('highlight', function (server) {
            // Overwrite server request to collect parse errors
            var request = server.request;

            server.request = function (doc, callback) {
                request.call(this, doc, function (err, data) {
                    if (err) {
                        data = { highlight: [] };
                        addError(data.highlight, err);
                    }

                    callback(null, data);
                });
            };

            return {};
        });
    };


    // Initialize the module
    if (typeof exports == 'object' && typeof module == 'object') {
        // CommonJS
        exports.initialize = function (ternDir) {
            var path = require('path');
            var acorn = require('acorn');
            var walk = require('acorn/dist/walk');
            var tern = require(path.resolve(ternDir, 'lib/tern'));

            initialize(tern, acorn, walk);
        };
    } else if (typeof define == 'function' && define.amd) {
        // AMD
        define(['tern/lib/tern', 'acorn', 'acorn/dist/walk'], initialize);
    } else {
        // globals
        initialize(tern, acorn, acorn.walk);
    }
}());
