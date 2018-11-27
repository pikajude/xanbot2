// @flow strict

import React from 'react';
import { Link } from 'react-router-dom';
import classnames from 'classnames';

import type { User } from 'Auth';

import css from 'main.css';
import foundation from 'foundation-sites/dist/css/foundation.css';
import fa from '@fortawesome/fontawesome-free/css/all.css';

type Props = {|
  user: User,
|};

type State = {| dropdownShown: boolean |};

class Header extends React.Component<Props, State> {
  state = { dropdownShown: false };

  _toggle = () => {
    this.setState(oldst => {
      oldst.dropdownShown = !oldst.dropdownShown;
      return oldst;
    });
  };

  _hide = () => {
    this.setState({ dropdownShown: false });
  };

  render() {
    const { user } = this.props;
    const active = this.state.dropdownShown;
    return (
      <div className={css.header}>
        <div
          className={classnames(css['header-overlay'], {
            [foundation.hide]: !active,
          })}
          onClick={this._hide}
        />
        <a
          className={classnames(foundation.dropdown, css['header-user'], {
            [css['header-active']]: active,
          })}
          onClick={this._toggle}
        >
          <span style={{ flexGrow: 1, display: 'flex', alignItems: 'center' }}>
            <img
              src={user.logo}
              alt={`${user.username}'s logo`}
              className={css['header-user-icon']}
            />
            {user.username}
          </span>
          <i
            className={classnames(fa['fa'], {
              [fa['fa-angle-down']]: !active,
              [fa['fa-angle-up']]: active,
            })}
          />
        </a>
        <ul
          className={classnames(
            foundation.menu,
            foundation.vertical,
            foundation.icons,
            foundation['icon-left'],
            {
              [foundation.hide]: !active,
              [css['header-dropdown']]: active,
            }
          )}
        >
          <li>
            <Link to="/logout">
              <i className={classnames(fa.fa, fa['fa-sign-out-alt'])} />
              <span>Log out</span>
            </Link>
          </li>
        </ul>
      </div>
    );
  }
}

export default Header;
