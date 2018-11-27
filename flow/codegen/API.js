// @flow strict
// generated code

export default {
  postAuthorize: async (body: {| tag: 'AuthCode', code: string
  |}): Promise<{| tag: 'Success',
     contents: void |} |
  {| tag: 'UserExists',
     contents: void |} |
  {| tag: 'Failure',
     contents: string |}> => {
    const opts = {
      method: "POST",
      body: JSON.stringify(body),
      headers: {
        "Content-Type": "application/json", 
      },
    };
  
    const resp = await fetch(`/api/authorize`, opts);
    if (resp.ok) {
      return await resp.json();
    } else if (resp.status === 401) {
      return Promise.reject(new Error('Unauthorized'));
    } else {
      return Promise.reject(new Error('Unknown response'));
    }
  }
  ,
  getCommands: async (headerCookie: string): Promise<Array<string>> => {
    const opts = {
      method: "GET",
      body: null,
      headers: {
        "Content-Type": "application/json", "Cookie": headerCookie
      },
    };
  
    const resp = await fetch(`/api/commands`, opts);
    if (resp.ok) {
      return await resp.json();
    } else if (resp.status === 401) {
      return Promise.reject(new Error('Unauthorized'));
    } else {
      return Promise.reject(new Error('Unknown response'));
    }
  }
}
