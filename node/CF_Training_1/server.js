const express = require('express');
const app = express();

app.get('/', (req, res) => res.send('Hello World, mister!'));
app.get('/info', (req, res) => res.send( process.env.VCAP_APPLICATION ));

app.listen( process.env.PORT || 4000);
