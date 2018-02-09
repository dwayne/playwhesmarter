const path = require('path');

module.exports = {
  entry: './assets/js/index.js',
  output: {
    filename: 'site.js',
    path: path.resolve(__dirname, 'output/js')
  }
};
