/*
 * @flow strict-local
 */

const msgpack = require('msgpack-lite');
const ab2b = require('arraybuffer-to-buffer');
import type { ServerMessage } from 'generated/serverTypes';

const socket = new WebSocket('ws://localhost:8080');

socket.addEventListener('open', event => false);

socket.addEventListener('close', event => console.log('Server closed connection'));

socket.addEventListener('message', event => {
  var p = document.createElement('p');
  const msg: ServerMessage = JSON.parse(event.data);
  console.log(msg);
  if (msg.tag === 'HANDSHAKE') {
    socket.send(
      JSON.stringify({
        tag: 'Login',
        jwtText:
          'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.e30._Ae-pdz285uLzyjHRY1binNuns09iVv7fAsIbjU_Bow',
      })
    );
  }
  p.innerHTML = `<code>${JSON.stringify(msg)}</code>`;
  document.body.prepend(p);
});
