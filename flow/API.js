// @flow strict
// @format

function post(
  path: string,
  body: mixed,
  init: ?RequestOptions
): Promise<Response> {
  return _handler('POST', path, body, init);
}

function get(path: string, init: ?RequestOptions): Promise<Response> {
  return _handler('GET', path, null, init);
}

function _handler(
  method: string,
  path: string,
  body: mixed,
  init: ?RequestOptions
): Promise<Response> {
  const opts = Object.assign(init || {}, {
    method,
    body: method != 'GET' ? JSON.stringify(body) : null,
    headers: { 'Content-Type': 'application/json', Cookie: document.cookie },
  });
  return fetch(`/api${path}`, opts);
}

export type AuthResponse =
  | { tag: 'Success' }
  | { tag: 'UserExists' }
  | { tag: 'Failure', contents: string };

export default { get, post };
