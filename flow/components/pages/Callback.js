// @flow strict

import type { ContextRouter } from 'react-router-dom';
import { Redirect } from 'react-router-dom';
import * as React from 'react';
import Cookies from 'js-cookie';

import API from 'API';
import type { AuthResponse as Response } from 'API';
import Auth from 'Auth';

type Props = {| ...ContextRouter |};

type State = { response: ?Response };

class Callback extends React.Component<Props, State> {
  state = { response: null };

  async componentDidMount() {
    if (Auth.getUser() != null) {
      this.setState({ response: { tag: 'UserExists' } });
    }

    let params = new URLSearchParams(location.search);
    const result = await API.post('/authorize', { code: params.get('code') });
    this.setState({ response: await result.json() });
  }

  render() {
    const resp = this.state.response;
    if (resp == null) {
      return <div>Authorizing you...</div>;
    } else if (resp.tag === 'Failure') {
      return <div>{resp.contents}</div>;
    } else {
      window.location = '/';
      return null;
    }
  }
}

export default Callback;
