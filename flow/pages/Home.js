// @flow strict

import type { ContextRouter } from 'react-router-dom';
import * as React from 'react';
import Uri from 'jsuri';

import API from '../API';

type Props = {|
  loggedIn: bool,
  ...ContextRouter,
|};

const _AUTH = new Uri('https://id.twitch.tv/oauth2/authorize')
  .addQueryParam('client_id', 'z92ktp9wj39vy8wc0t8q2k52q41pb3')
  .addQueryParam('redirect_uri', 'http://localhost:8080/callback')
  .addQueryParam('response_type', 'code')
  .addQueryParam('scope', 'user_read channel_editor')
  .toString();

class Home extends React.Component<Props> {
  render() {
    if (this.props.loggedIn) {
      return <div>Welcome back</div>;
    } else {
      return <a href={_AUTH}>Authorize xanbot &#x1f916;</a>;
    }
  }
}

export default Home;
