const express = require('express');
const app = express();

app.get('/api/employees', function (req,res){
    res.setHeader('Content-Type', 'application/json');
    console.log(req.get('Access'));
    switch (req.get('Access')){
     case undefined:
        v1_response(res);
        break;
    case "v1":
        v1_response(res);
        break;
    case "v2":
        v2_response(res);
        break;
    }
});

function v1_response(res){
    res.send({version: "v1"})
}

function v2_response(res){
    res.send({version: "v2"})
}

app.listen( process.env.PORT || 3000);
