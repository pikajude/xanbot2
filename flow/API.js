// @flow strict
// @format

function post(path: string, body: mixed, init: ?RequestOptions) {
  return _handler('POST', path, body, init);
}

function get(path: string, init: ?RequestOptions) {
  return _handler('GET', path, null, init);
}

async function _handler(
  method: string,
  path: string,
  body: mixed,
  init: ?RequestOptions
): Promise<{}> {
  const opts = Object.assign(init || {}, {
    method,
    body: method != 'GET' ? JSON.stringify(body) : null,
    headers: { 'Content-Type': 'application/json', Cookie: document.cookie },
  });
  const resp = await fetch(`/api${path}`, opts);
  if (resp.ok) {
    return await resp.json();
  } else if (resp.status === 401) {
    return Promise.reject(new Error('Unauthorized'));
  }
}

export type AuthResponse =
  | { tag: 'Success' }
  | { tag: 'UserExists' }
  | { tag: 'Failure', contents: string };

export default { get, post };
