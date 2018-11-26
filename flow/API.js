// @flow strict
function post(path: string, body: mixed, init: ?RequestOptions): Promise<
  Response,
> {
  return _handler('POST', path, body, init);
}

function get(path: string, init: ?RequestOptions): Promise<Response> {
  return _handler('GET', path, null, init);
}

function _handler(method: string, path: string, body: mixed, init: ?RequestOptions): Promise<Response> {
  const opts = Object.assign(
    init || {  },
    {
      method,
      body: method != 'GET' ? JSON.stringify(body) : null,
      headers: { "Content-Type": "application/json", "Cookie": document.cookie },
    },
  );
  return fetch(path, opts);
}

export default { get, post };

