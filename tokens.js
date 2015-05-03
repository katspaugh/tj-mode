/**
 * @see https://github.com/katspaugh/tj-mode
 */
(function (mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
        return mod(require("tern/lib/infer"), require("tern/lib/tern"), require('acorn'), require("acorn/dist/walk"));
    if (typeof define == "function" && define.amd) // AMD
        return define(["tern/lib/infer", "tern/lib/tern", "acorn", "acorn/dist/walk"], mod);
    mod(tern, tern, acorn.walk);
})(function (infer, tern, acorn, walk) {
    'use strict';

    function getNodeName(node) {
        if (node.callee) {
            // This is a CallExpression node.
            // We get the position of the function name.
            return getNodeName(node.callee);
        } else if (node.property) {
            // This is a MemberExpression node.
            // We get the name of the property.
            return node.property.name;
        } else {
            return node.name;
        }
    }

    function addMessage(messages, node, type, msg) {
        if (!node) { return; }

        var payload = {
            type: type || node.type,
            start: node.start,
            end: node.end
        };

        if (msg) {
            payload.message = msg;
        }

        messages.push(payload);
    }


    function makeVisitors (server, query, file, messages) {
        var add = function (node, type, msg) {
            addMessage(messages, node, type, msg);
        };

        return {
            VariableDeclaration: function (node) {
                node.declarations.forEach(function (decl) {
                    add(decl.id);
                });
            },

            Function: function (node) {
                node.params.forEach(function (param) {
                    add(param, 'ArgumentName');
                });

                add(node.id, 'FunctionName');
            },

            ThisExpression: add,

            Property: function (node) {
                add(node.key);
            },

            Literal: function (node) {
                if (typeof node.value === 'number') {
                    add(node, 'NumberLiteral');
                } else {
                    add(node);
                }
            },

            // Detects top-level identifiers, e.g. the object in
            // `object.property` or just `object`.
            Identifier: function (node, state, c) {
                // FIXME
                if (node.value === '✖' || node.name === '✖') {
                    add(node, 'Error', 'Syntax error');
                    return;
                }

                var type = infer.expressionType({
                    node: node, state: state
                });

                if (type.originNode != null || type.origin != null) {
                    // The node is defined somewhere (could be this node),
                    // regardless of whether or not the type is known.
                } else if (type.isEmpty()) {
                    // The type of the identifier cannot be determined,
                    // and the origin is unknown.
                    add(node, 'Error', 'Unknown identifier ' + getNodeName(node));
                } else {
                    // Even though the origin node is unknown, the type is known.
                    // This is typically the case for built-in identifiers (e.g. window or document).
                }
            }
        };
    }

    var scopeVisitor = walk.make({
        Function: function(node, _st, c) {
            var scope = node.body.scope;
            if (node.id) c(node.id, scope);
            node.params.forEach(function (param) {
                c(param, scope);
            });
            c(node.body, scope, 'ScopeBody');
        }
    });

    tern.defineQueryType('tokens', {
        run: function (server, query) {
            var messages = {};

            server.files.forEach(function (file) {
                var fileMessages = messages[file.name] = [];

                // Add tokens
                var comments = [];
                var tokens = [];
                var tokenizer = acorn.tokenizer(file.text, {
                    onComment: comments
                });

                while (true) {
                    var token = tokenizer.getToken();

                    if (token.type.keyword) {
                        addMessage(fileMessages, {
                            type: 'Keyword',
                            start: token.start,
                            end: token.end
                        });
                    }

                    if (token.type.label === 'eof') {
                        break;
                    }
                }

                comments.forEach(function (comment) {
                    addMessage(fileMessages, {
                        type: 'Comment',
                        start: comment.start,
                        end: comment.end
                    });
                });

                var visitors = makeVisitors(server, query, file, fileMessages);
                walk.simple(file.ast, visitors, scopeVisitor, file.scope);

                // Collect syntax errors
                // FIXME: find a way to avoid re-parsing just for errors
                try {
                    var ast = acorn.parse(file.text, {
                        onComment: comments
                    });
                } catch (e) {
                    addMessage(fileMessages, {
                        type: 'Error',
                        message: 'Syntax error: ' + e.message,
                        start: e.pos,
                        end: e.raisedAt
                    });
                }
            });

            return messages;
        }
    });

    tern.registerPlugin('tokens', function (server) {
        return {};
    });
});
