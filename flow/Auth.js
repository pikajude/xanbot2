// @flow strict

import Cookies from 'js-cookie';

export type Payload = {
  username: string,
  logo: string,
};

export default {
  getUser(): ?Payload {
    const sess = Cookies.get('_SESSION');
    if (sess == null) {
      return null;
    }
    const data1 = sess.split('.')[1];
    if (data1 == null) {
      return null;
    }
    return JSON.parse(window.atob(data1.replace('-', '+').replace('_', '/')));
  },

  loggedIn(): bool {
    return this.getUser() != null;
  }
};
