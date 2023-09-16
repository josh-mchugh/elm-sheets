import './style.css';
import { Elm } from './src/Main.elm';

Elm.Main.init({
    node: document.getElementById('app'),
    flags: Math.floor(Math.random()*0x0FFFFFFF)
});
