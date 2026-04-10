---
name: afip-invoice
description: Issue a monthly Factura C on AFIP/ARCA for Monotributo. Use when the user says "afip invoice", "issue invoice", "factura", "facturar", or "emitir factura".
user-invocable: true
---

# AFIP monthly invoice (Factura C)

Issue a Factura C via ARCA's RCEL (Comprobantes en Línea) system. This skill is run once per month.

## Configuration

Before starting, read `~/.config/afip/invoice-defaults.json` to load all invoice parameters (emisor/receptor details, amounts, activity codes). All field values referenced below as `config.<section>.<field>` come from this file.

## Date conventions

All dates use DD/MM/YYYY format (Argentine convention).

- **Fecha del Comprobante**: 1st day of the current month
- **Período Facturado Desde**: 1st day of the previous month
- **Período Facturado Hasta**: last day of the previous month
- **Vto. para el Pago**: 1st day of the current month
- **Fecha de Emisión**: 1st day of the current month

## Login

1. Navigate to: `https://auth.afip.gov.ar/contribuyente_/login.xhtml?action=SYSTEM&system=admin_mono`
2. Enter `config.emisor.cuit` in the CUIT/CUIL field.
3. Click "Siguiente".
4. **STOP and ask the user to enter their Clave Fiscal password manually.** Do not attempt to enter the password.
5. Wait for the user to confirm they are logged in.

## Navigate to invoice form

1. Click **"Facturación"** in the left sidebar.
2. Click **"Emitir Factura"** (this opens a new tab with RCEL).
3. In the new RCEL tab, click the emisor name (`config.emisor.nombre`) to select the company.
4. Click **"Generar Comprobantes"**.

## Step 1 of 4: Punto de Venta y Tipo de Comprobante

- **Punto de Ventas a utilizar**: `config.emisor.punto_de_venta`
- **Tipo de Comprobante**: `config.comprobante.tipo`
- Click **Continuar**.

## Step 2 of 4: Datos de Emisión

- **Fecha del Comprobante**: 1st day of current month
- **Conceptos a incluir**: `Servicios`
- After selecting Servicios, date fields appear:
  - **Desde**: 1st day of previous month
  - **Hasta**: last day of previous month
  - **Vto. para el Pago**: 1st day of current month
- **Actividades Asociadas**: select `config.comprobante.actividad`
- Click **Continuar**.

## Step 3 of 4: Datos del Receptor

- **Condición frente al IVA**: `config.receptor.condicion_iva`
- **Tipo y Nro. de Documento**: CUIT (auto-selected after choosing IVA condition). For the CUIT number, **click on the input field and type `config.receptor.cuit` using keyboard input** (do NOT use form_input for this field), then press **Tab** to trigger the auto-population of name and address. Wait 2-3 seconds for the AJAX call.
- **A. y Nombre o Razón Social**: auto-populated as `config.receptor.nombre` (verify this appeared).
- **Domicilio Comercial**: auto-populated (verify this appeared).
- **Email**: leave blank.
- **Condiciones de Venta**: check **Contado**. This checkbox is fragile — the form likely checks `event.isTrusted` so only real (non-scripted) click events register. Follow this protocol strictly:
  1. Do all other programmatic work on this page first (IVA selection, CUIT entry, AJAX wait).
  2. Use `computer` `left_click` with the checkbox's `ref` ID (a single clean CDP click — do NOT use `form_input`, JavaScript `.click()`, or `.checked = true`). The checkbox must not have been touched by any other method before this click.
  3. **Verify visually** (zoom screenshot) that Contado is checked.
  4. Click **Continuar** immediately. **Do not perform any other programmatic interaction on this page after checking Contado** — any further action (e.g. resetting dropdowns) may invalidate the checkbox state.
  5. On the summary page, verify that Condiciones de Venta shows "Contado" (not "null"). If it shows "null", the CDP click didn't work — fall back to asking the user to tick Contado manually (do all programmatic work first, then STOP and ask the user, then click Continuar immediately after they confirm).

## Step 4 of 4: Datos de la Operación

- **Producto/Servicio**: `config.comprobante.producto_servicio`
- **Cant.**: `config.comprobante.cantidad`
- **U. Medida**: explicitly select `config.comprobante.unidad_medida` (it does NOT default to this — you must select it manually)
- **Prec. Unitario**: `config.comprobante.precio_unitario`
- **Verify visually** that U. Medida shows the configured unit (not "seleccionar...").
- Click **Continuar**.

## Step 5 of 4: Resumen de Datos — MANDATORY VERIFICATION

Before clicking "Confirmar Datos...", you **MUST** read the full page DOM using `read_page` with `filter: all` and verify **every single field** against the expected values from the config file:

| Field | Expected value |
|---|---|
| Razón Social (emisor) | `config.emisor.nombre` |
| Punto de Venta | `config.emisor.punto_de_venta` |
| Domicilio | `config.emisor.domicilio` |
| Conceptos a Incluir | Servicios |
| Período Facturado desde | 1st of previous month |
| Período Facturado hasta | last day of previous month |
| Vto. para el Pago | 1st of current month |
| CUIT (receptor) | `config.receptor.cuit` |
| Razón Social (receptor) | `config.receptor.nombre` |
| Domicilio Comercial (receptor) | `config.receptor.domicilio` |
| Condición frente al IVA | `config.receptor.condicion_iva` |
| Condiciones de Venta | Contado |
| Producto/Servicio | `config.comprobante.producto_servicio` |
| Cant. | formatted from `config.comprobante.cantidad` |
| U. Medida | `config.comprobante.unidad_medida` |
| Prec. Unitario | formatted from `config.comprobante.precio_unitario` |
| Importe Total | formatted from `config.comprobante.precio_unitario` * `config.comprobante.cantidad` |

**If ANY field does not match, DO NOT click "Confirmar Datos...".** Report the discrepancy and ask the user how to proceed.

**If ALL fields match**, ask the user for explicit confirmation before clicking "Confirmar Datos...".

## Important technical notes

- This is an old government website. Use `form_input` for dropdowns and text fields, but use **keyboard typing** (click + type action) for the CUIT number field — programmatic value setting does not trigger the AJAX auto-populate.
- The Contado checkbox requires a clean CDP physical click (`computer` `left_click` with ref) — `form_input`, JavaScript `.checked`, and `.click()` all fail silently (the form likely checks `event.isTrusted`). If the CDP click also fails, fall back to asking the user to tick it manually. See Step 3 for the exact protocol.
- The U. Medida dropdown must be explicitly set — it does not default.
- After each page transition, wait 2 seconds and take a screenshot to confirm the page loaded.
- If the browser extension disconnects, ask the user to reconnect and retry.
