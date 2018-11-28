// @flow strict

import type { ContextRouter } from 'react-router-dom';
import * as React from 'react';
import Uri from 'jsuri';

import Auth from 'Auth';

import foundation from 'foundation-sites/dist/css/foundation.css';

type Props = {
  ...ContextRouter,
};

const _AUTH = new Uri('https://id.twitch.tv/oauth2/authorize')
  .addQueryParam('client_id', 'z92ktp9wj39vy8wc0t8q2k52q41pb3')
  .addQueryParam('redirect_uri', 'http://localhost:8080/callback')
  .addQueryParam('response_type', 'code')
  .addQueryParam('scope', 'user_read channel_editor')
  .toString();

class Home extends React.Component<Props> {
  componentDidMount() {
    document.title = 'Home';
  }

  render() {
    if (Auth.loggedIn()) {
      return (
        <div>
          <div className={foundation.card}>
            <div className={foundation['card-divider']}>Some stat card</div>
            <div className={foundation['card-section']}>
              <code>{JSON.stringify(Auth.getUser())}</code>
            </div>
          </div>
          <div className={foundation.card}>
            <div className={foundation['card-divider']}>
              Some other stat card
            </div>
          </div>
        </div>
      );
    } else {
      return <a href={_AUTH}>Authorize xanbot &#x1f916;</a>;
    }
  }
}

export default Home;
