const Main = require('./bin/Main');
const express = require('express');

const app = express();

app.get('/', function (req, res) {
  res.send(Main.main);
});

app.listen(8080, function () {
  console.log('now serving at http://localhost:8080');
});
