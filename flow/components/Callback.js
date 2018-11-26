// @flow strict

import type { ContextRouter } from 'react-router-dom';
import { Redirect } from 'react-router-dom';
import * as React from 'react';
import Cookies from 'js-cookie';

import API from '../API';
import Auth from '../Auth';

type Props = {|
  ...ContextRouter,
|};

type Response =
  | {
      status: 'SUCCESS',
      code: string,
    }
  | {
      status: 'ERROR',
      message: string,
    };

type State = {| response: ?string, loaded: boolean |};

class Callback extends React.Component<Props, State> {
  state = { response: null, loaded: false };

  async componentDidMount() {
    if (Auth.getUser() != null) {
      return;
    }

    let params = new URLSearchParams(location.search);
    const result = await API.post('/authorize', { code: params.get('code') });
    this.setState({ response: await result.json(), loaded: true });
  }

  render() {
    if (!this.state.loaded) {
      return <div>Loading...</div>;
    } else {
      const resp = this.state.response;
      if (resp === null) {
        // success
        window.location = '/';
        return null;
      } else {
        return <div>{resp}</div>;
      }
    }
  }
}

export default Callback;
