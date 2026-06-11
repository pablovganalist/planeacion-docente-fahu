// =============================================================================
// netlify/functions/identity-login.js
//
// Webhook de Netlify Identity que se ejecuta en CADA LOGIN.
// Sincroniza el rol del usuario con las variables de entorno actuales,
// lo que corrige a usuarios que se registraron antes de ser agregados
// a la lista de admins o cuyo rol haya cambiado desde el registro.
// =============================================================================

const UNIDADES = [
  { envVar: "EMAILS_ADMIN",              rol: "admin" },
  { envVar: "EMAILS_EDUCACION",          rol: "educacion" },
  { envVar: "EMAILS_ESTUDIOS_POLITICOS", rol: "estudios_politicos" },
  { envVar: "EMAILS_FILOSOFIA",          rol: "filosofia" },
  { envVar: "EMAILS_HISTORIA",           rol: "historia" },
  { envVar: "EMAILS_LINGUISTICA",        rol: "linguistica" },
  { envVar: "EMAILS_PERIODISMO",         rol: "periodismo" },
  { envVar: "EMAILS_PSICOLOGIA",         rol: "psicologia" },
];

function parseCorreos(envVar) {
  const raw = process.env[envVar] || "";
  return new Set(
    raw.split(",")
       .map(e => e.trim().toLowerCase())
       .filter(e => e.length > 0)
  );
}

exports.handler = async (event) => {
  let payload;
  try {
    payload = JSON.parse(event.body);
  } catch {
    return { statusCode: 400, body: "Payload inválido" };
  }

  const email = (payload?.user?.email || "").trim().toLowerCase();

  if (!email) {
    return { statusCode: 200, body: JSON.stringify({}) };
  }

  let rolAsignado = null;

  for (const unidad of UNIDADES) {
    const correos = parseCorreos(unidad.envVar);
    if (correos.has(email)) {
      rolAsignado = unidad.rol;
      break;
    }
  }

  if (!rolAsignado) {
    // Usuario no autorizado — no actualizar roles (identity-validate ya bloqueó el registro)
    return { statusCode: 200, body: JSON.stringify({}) };
  }

  // Devuelve el rol correcto según las variables de entorno actuales
  return {
    statusCode: 200,
    body: JSON.stringify({
      app_metadata: {
        roles: [rolAsignado],
      },
    }),
  };
};
