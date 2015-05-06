/**
 * @see https://github.com/katspaugh/tj-mode
 */
(function (mod) {
    if (typeof exports == 'object' && typeof module == 'object') // CommonJS
        return mod(require('tern/lib/infer'), require('tern/lib/tern'), require('acorn'), require('acorn/dist/walk'));
    if (typeof define == 'function' && define.amd) // AMD
        return define(['tern/lib/infer', 'tern/lib/tern', 'acorn', 'acorn/dist/walk'], mod);
    mod(tern, tern, acorn, acorn.walk);
})(function (infer, tern, acorn, walk) {
    'use strict';

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

    function makeVisitors (server, query, file, collect) {
        return {
            VariableDeclaration: function (node) {
                for (var i = 0, len = node.declarations.length; i < len; i++) {
                    var decl = node.declarations[i];

                    if (decl.init && decl.init.type === 'FunctionExpression') {
                        collect(decl.id, 'FunctionIdentifier');
                    } else {
                        collect(decl.id);
                    }
                }
            },

            Function: function (node) {
                for (var i = 0, len = node.params.length; i < len; i++) {
                    collect(node.params[i], 'ArgumentIdentifier');
                }

                collect(node.id, 'FunctionIdentifier');
            },

            BlockStatement: function (node) {
                collect(node, 'BlockStatement');
            },

            ThisExpression: function (node) {
                collect(node, 'SpecialIdentifier');
            },

            Property: function (node) {
                if (node.value && node.value.type === 'FunctionExpression') {
                    collect(node.key, 'FunctionIdentifier');
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
            },

            /**
             * @see https://github.com/angelozerr/tern-lint
             *
             * Detects top-level identifiers, e.g. the object in
             * `object.property` or just `object`.
             */
            Identifier: function (node, state, c) {
                var type = infer.expressionType({
                    node: node, state: state
                });

                if (type.originNode == null && type.origin == null && type.isEmpty()) {
                    // The type of the identifier cannot be determined,
                    // and the origin is unknown.
                    collect({
                        type: 'Warning',
                        start: node.start,
                        end: node.end,
                        message: 'Unknown identifier ' + node.name
                    });
                }
            }
        };
    }

    /**
     * @see https://github.com/angelozerr/tern-lint
     */
    var scopeVisitor = walk.make({
        Function: function(node, _st, c) {
            var scope = node.body.scope;
            if (node.id) c(node.id, scope);
            for (var i = 0, len = node.params.length; i < len; i++) {
                c(node.params[i], scope);
            }
            c(node.body, scope, 'ScopeBody');
        }
    });

    tern.defineQueryType('highlight', {
        run: function (server, query) {
            var messages = {};

            server.files.forEach(function (file) {
                var fileMessages = messages[file.name] = [];

                var collect = function (node, type) {
                    addMessage(fileMessages, node, type);
                };

                // Walk the error-tolerant parser AST
                var visitors = makeVisitors(server, query, file, collect);
                walk.simple(file.ast, visitors, scopeVisitor, file.scope);

                // Re-parse to collect keyword tokens, comments, warnings and errors
                try {
                    var ast = acorn.parse(file.text, {
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
                } catch (err) {
                    addError(fileMessages, err);
                }
            });

            return messages;
        }
    });

    tern.registerPlugin('highlight', function (server) {
        // Overwrite server request to collect parse errors
        // For some reason, not all errors are caught here
        var request = server.request;

        server.request = function (doc, callback) {
            request.call(this, doc, function (err, data) {
                if (err) {
                    data = {};
                    data[doc.query.file] = [];
                    addError(data[doc.query.file], err);
                }

                callback(null, data);
            });
        };

        return {};
    });
});
