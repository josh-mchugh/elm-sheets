import './style.css';
import { Elm } from './src/Main.elm';
import layout from './layout.json';
import resume from './resume.json';

Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        externalRandom: Math.floor(Math.random()*0x0FFFFFFF),
        layout: layout,
        resume: resume
    }
});
