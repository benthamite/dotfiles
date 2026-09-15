"use strict";

const http = require("node:http");

const pages = {
  "/plain": `<!doctype html><html><body><main>Plain fixture content</main></body></html>`,
  "/cookieconsent2": `<!doctype html>
    <html class="show--consent"><body>
      <main id="content" hidden>Unlocked fixture content</main>
      <div id="cc--main"><div id="cm" role="dialog" aria-modal="true">
        <p>Choose your cookie preferences</p>
        <button id="s-all-bn">Accept all</button>
        <button id="s-rall-bn">Reject all</button>
      </div></div>
      <script>
        document.querySelector('#s-rall-bn').addEventListener('click', () => {
          document.cookie = 'cc_cookie=necessary; SameSite=Lax';
          document.documentElement.classList.remove('show--consent');
          document.querySelector('#cc--main').remove();
          document.querySelector('#content').hidden = false;
          document.body.dataset.consent = 'opted-out';
        });
      </script>
    </body></html>`,
  "/unknown": `<!doctype html><html><body>
    <main>Blocked fixture content</main>
    <div role="dialog" aria-modal="true" style="position:fixed;inset:0;z-index:9999;background:white">
      <p>We use cookies. Choose your privacy preferences.</p>
      <button>Continue</button>
    </div>
  </body></html>`,
  "/cookie-state": `<!doctype html><html><body><main id="cookies"></main>
    <script>document.querySelector('#cookies').textContent = document.cookie || 'no-cookies';</script>
  </body></html>`,
  "/residual": `<!doctype html><html><body><main>Residual fixture content</main>
    <div class="newsletter-overlay" role="dialog" aria-modal="true"
         style="position:fixed;inset:10%;z-index:100;background:white">
      <p>Subscribe to our newsletter</p><button aria-label="Close">Close</button>
    </div>
  </body></html>`,
  "/subscribe-dialog": `<!doctype html><html><body>
    <article class="typography newsletter-post post">Subscribe fixture content</article>
    <div style="position:fixed;inset:0;z-index:100;background:rgba(0,0,0,.5)">
      <div role="dialog" style="position:absolute;top:20%;left:30%;width:40%;background:white">
        <p>Discover more from the fixture</p><button>Subscribe</button>
        <p>By subscribing, you agree to our Terms of Use and Privacy Policy.</p>
      </div>
    </div>
  </body></html>`,
  "/never-ready": `<!doctype html><html><body><script>
    document.body.textContent = '';
  </script></body></html>`,
  "/challenge": `<!doctype html><html><head><title>Just a moment...</title></head>
    <body><main><h1>Performing security verification</h1>
      <p>This website verifies you are not a bot.</p><p>Verifying...</p>
    </main></body></html>`,
  "/dynamic": `<!doctype html><html><body><main id="content">Loading</main>
    <script>fetch('/dynamic-data').then(response => response.text()).then(text => {
      document.querySelector('#content').textContent = text;
    });</script>
  </body></html>`,
  "/newsletter-content": `<!doctype html><html><body><main id="maincontent">
    <article>Important article body</article>
    <section id="newsletters">Newsletter recommendations</section>
  </main></body></html>`,
};

function startFixtureServer() {
  const server = http.createServer((request, response) => {
    const pathname = new URL(request.url, "http://localhost").pathname;
    if (pathname === "/dynamic-data") {
      setTimeout(() => {
        response.writeHead(200, { "content-type": "text/plain" });
        response.end("Dynamic article content arrived");
      }, 1800);
      return;
    }
    const page = pages[pathname];
    response.writeHead(page ? 200 : 404, { "content-type": "text/html" });
    response.end(page || "Not found");
  });
  return new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address();
      resolve({
        origin: `http://127.0.0.1:${port}`,
        close: () => new Promise((done) => server.close(done)),
      });
    });
  });
}

module.exports = { startFixtureServer };
