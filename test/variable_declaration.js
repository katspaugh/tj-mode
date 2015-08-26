var util = require('./util');

exports['test simple variable declaration'] = function () {
  util.assertHighlight(
      'var a = 10;',

      {
          'highlight': [
              { 'type': 'Keyword', 'start': 0, 'end': 3 },
              { 'type': 'NumberLiteral', 'start': 8, 'end': 10 },
              { 'type': 'VariableDeclaration', 'start': 4, 'end': 5 },
          ]
      }
  );
}

if (module == require.main) require('test').run(exports);
