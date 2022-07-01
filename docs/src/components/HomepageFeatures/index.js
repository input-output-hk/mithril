import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
    {
        title: 'Modular',
        Svg: require('@site/static/img/card-operate-a-stake-pool.svg').default,
        description: (
            <>
                Mithril is a modular set of tools to accelerate blockchain synchronization.
                Find components description in Topologies.
            </>
        ),
        link: '/doc/category/topologies',
    },
    {
        title: 'How it works',
        Svg: require('@site/static/img/card-native-tokens.svg').default,
        description: (
            <>
                How does the heart of Mithril work?
                Find how the Mithril multisignature works in Core Concepts.
            </>
        ),
        link: '/doc/category/core-concepts',
    },
    {
        title: 'Get started',
        Svg: require('@site/static/img/card-get-started.svg').default,
        description: (
            <>
                Quick instructions to build it.
            </>
        ),
        link: '/doc/manual/getting-started/welcome',
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
