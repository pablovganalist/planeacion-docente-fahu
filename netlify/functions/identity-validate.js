// =============================================================================
// netlify/functions/identity-validate.js
//
// Webhook de validación de Netlify Identity.
// Se ejecuta ANTES de que se cree la cuenta del usuario.
//
// - Verifica que el correo esté en la lista autorizada de alguna unidad.
// - Si está autorizado → aprueba y asigna el rol correspondiente.
// - Si no está → rechaza con mensaje claro.
//
// Variables de entorno requeridas (Netlify → Site configuration → Env variables):
//   EMAILS_EDUCACION            correo1@usach.cl,correo2@usach.cl,...
//   EMAILS_ESTUDIOS_POLITICOS   correo1@usach.cl,...
//   EMAILS_FILOSOFIA            correo1@usach.cl,...
//   EMAILS_HISTORIA             correo1@usach.cl,...
//   EMAILS_LINGUISTICA          correo1@usach.cl,...
//   EMAILS_PERIODISMO           correo1@usach.cl,...
//   EMAILS_PSICOLOGIA           correo1@usach.cl,...
//   EMAILS_ADMIN                correo1@usach.cl,...  (opcional, acceso total)
// =============================================================================

// Mapa de variable de entorno → rol de Netlify Identity
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

// Parsea la variable de entorno en un Set de correos normalizados
function parseCorreos(envVar) {
  const raw = process.env[envVar] || "";
  return new Set(
    raw.split(",")
       .map(e => e.trim().toLowerCase())
       .filter(e => e.length > 0)
  );
}

exports.handler = async (event) => {
  // Netlify envía el evento como JSON en el body
  let payload;
  try {
    payload = JSON.parse(event.body);
  } catch {
    return { statusCode: 400, body: "Payload inválido" };
  }

  const email = (payload?.user?.email || "").trim().toLowerCase();

  if (!email) {
    return {
      statusCode: 422,
      body: JSON.stringify({ error: "No se recibió correo electrónico." }),
    };
  }

  // Buscar en qué unidad está autorizado este correo
  let rolAsignado = null;

  for (const unidad of UNIDADES) {
    const correos = parseCorreos(unidad.envVar);
    if (correos.has(email)) {
      rolAsignado = unidad.rol;
      break; // primer match gana
    }
  }

  if (!rolAsignado) {
    // Correo no autorizado → rechazar registro
    return {
      statusCode: 422,
      body: JSON.stringify({
        error:
          "Tu cuenta no está autorizada para acceder a este sistema. " +
          "Si crees que deberías tener acceso, contacta al Vicedecanato de Docencia.",
      }),
    };
  }

  // Correo autorizado → aprobar y asignar rol
  return {
    statusCode: 200,
    body: JSON.stringify({
      app_metadata: {
        roles: [rolAsignado],
      },
    }),
  };
};
