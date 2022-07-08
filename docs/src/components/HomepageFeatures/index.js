import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'User Manual',
    Svg: require('@site/static/img/card-get-started.svg').default,
    description: (
      <>
        Get Started, guides, installation instructions and developer docs.
      </>
    ),
    link: '/doc/manual/welcome',
  },
  {
    title: 'The Mithril Protocol',
    Svg: require('@site/static/img/card-native-tokens.svg').default,
    description: (
      <>
        How does the Mithril Protocol work?
        Find more about the Mithril multi signature.
      </>
    ),
    link: '/doc/mithril/intro',
  },
  {
    title: 'The Mithril Network',
    Svg: require('@site/static/img/card-operate-a-stake-pool.svg').default,
    description: (
      <>
        What is the Mithril Network?
        Find more about all the nodes involved the network and its architecture.
      </>
    ),
    link: '/doc/manual/developer-docs/architecture/',
  },
];

function Feature({ Svg, title, description, link }) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <a href={link}><Svg className={styles.featureSvg} role="img" /></a>
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
