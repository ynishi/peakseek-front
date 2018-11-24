'use strict';

require('./index.html');
const { Elm } = require('./Main.elm');

var app = Elm.Main.init({flags: process.env.API_URL, node: document.getElementById("app")});
