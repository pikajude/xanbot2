// @flow strict

import * as React from 'react';
import { Link } from 'react-router-dom';
import type { ContextRouter } from 'react-router-dom';
import classnames from 'classnames';

import css from '../../css/main.css';
import foundation from '../../css/foundation.css';

export default function(props: ContextRouter) {
  let activated = function(path: string) {
    if (path === props.location.pathname) {
      return { className: foundation['is-active'] };
    }
    return {};
  };
  return (
    <div className={css.navbar}>
      <Link id={css['home-button']} to="/">
        &#x1f916;
      </Link>
      <ul className={classnames(foundation.vertical, foundation.menu)}>
        <li {...activated('/commands')}>
          <Link to="/commands">Commands</Link>
        </li>
        <li {...activated('/keywords')}>
          <Link to="/keywords">Keywords</Link>
        </li>
      </ul>
    </div>
  );
}
