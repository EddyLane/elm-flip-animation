import './main.css';
import {Elm} from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('root')
});

app.ports.getBoundingClientRects.subscribe((ids) => {

    console.log('[elm-flip-animation] getBoundingClientRects', ids);

    requestAnimationFrame(() => app.ports.gotBoundingClientRects.send(
        ids.reduce((acc, id) => {

            const el = document.querySelector(`[data-elm-flip-id="${id}"]`);

            if (el) {
                acc.push({
                    id, rectangle: {
                        left: el.offsetLeft,
                        top: el.offsetTop,
                        height: el.offsetHeight,
                        width: el.offsetWidth
                    }
                });
            }

            return acc;

        }, [])
    ));

});
