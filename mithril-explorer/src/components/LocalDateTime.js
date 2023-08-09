import React from 'react';

export default function LocalDateTime({datetime}) {
  return (<>{new Date(datetime).toLocaleString()}</>);
}
