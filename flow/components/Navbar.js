// @flow strict

import * as React from 'react';
import { Link } from 'react-router-dom';
import type { ContextRouter } from 'react-router-dom';
import classnames from 'classnames';

import css from 'main.css';
import foundation from 'foundation-sites/dist/css/foundation.css';

export default function(props: ContextRouter) {
  let active = function(path: string) {
    return {
      className: classnames({
        [foundation.active]: path === props.location.pathname,
      }),
    };
  };
  return (
    <div className={css.navbar}>
      <Link id={css['home-button']} to="/">
        &#x1f916;
      </Link>
      <div className={css['navbar-options']}>
        <ul
          className={classnames(
            foundation.menu,
            foundation.vertical,
            foundation.dropdown
          )}
        >
          <li {...active('/commands')}>
            <Link to="/commands">Commands</Link>
          </li>
          <li {...active('/keywords')}>
            <Link to="/keywords">Keywords</Link>
          </li>
        </ul>
      </div>
    </div>
  );
}
